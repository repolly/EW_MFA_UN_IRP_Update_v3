##### Harmonize data from UNSD
  ### Steps:
    # Add alphanumiso
      # Check for missing allocations
    # Add material_ids
      # Check for missing allocations
    # Add unit_ids
      # Check for missing allocations
    # Adjust "Other hydrocarbons"
    # Aggregate






library(tidyverse)
library(readxl)

# clear R environment
rm(list = ls())




## read files

# data
unsd <- read_delim(
  "./03_intermediate/01_data_retrieval/unsd/UN Energy Stats DB as of 29Sept22.txt",#"./03_intermediate/01_data_retrieval/unsd/UN Energy Stats DB 1950-2018.txt",
  delim = "|",
  col_names = c("country", "product", "flow", "transfer", "import", "year", "unit", "flag", "value"),
  col_types = cols(value = "n",
                   country = "c",
                   year = "i",
                   import = "c",
                   transfer = "c"
                   )
  )

# concordance tables
source_cou_ids <- read_delim("./01_input/01_concordance_tables/source_country_ids.csv", delim = ";")

source_mat_ids <- read_delim("./01_input/01_concordance_tables/source_material_ids.csv", delim = ";")

source_unit_ids <- read_delim("./01_input/01_concordance_tables/source_unit_ids.csv", delim = ";")





## format

# filter for production (i.e. flow "01")
unsd <- unsd %>% 
  filter(flow == "01")




## add country_ids
unsd <- unsd %>% 
  left_join(.,
            source_cou_ids %>% 
              filter(source == "UNSD_Code") %>% 
              select(-source),
            by = c("country" = "source_country_id")
            )

# no_match table
  ## (including country names from UNSD)

# read country code table of UNSD
unsd_cou_codes <- read_xlsx(path = "./03_intermediate/01_data_retrieval/unsd/Energy DB Codes 2019.xlsx", 
                            sheet = "Countries",
                            skip = 1)

no_match_cou <- unsd %>%
  filter(is.na(alphanumiso)) %>%
  distinct(country) %>%
  mutate(across(country, as.numeric)) %>%
  left_join(.,
            unsd_cou_codes, 
            by = c("country" = "Code")
            )

# save csv
write_delim(no_match_cou, file = "./04_output/01_harmonization/01_conc_nomatch/unsd_no_match_cou.csv")




## add mat_ids
unsd <- unsd %>% 
  left_join(.,
            source_mat_ids %>% 
              filter(source == "UNSD_Code") %>% 
              select(-source),
            by = c("product" = "source_material_id")
  )

# no_match table
  # (including commodity names from UNSD)

# read country code table of UNSD
unsd_mat_codes <- read_xlsx(path = "./03_intermediate/01_data_retrieval/unsd/Energy DB Codes 2019.xlsx", 
                            sheet = "Products")

no_match_mat <- unsd %>%
  filter(is.na(material_id)) %>%
  distinct(product) %>%
  left_join(.,
            unsd_mat_codes %>%
              select(Code, Name), 
            by = c("product" = "Code")
  )

# save csv
write_delim(no_match_mat, file = "./04_output/01_harmonization/01_conc_nomatch/unsd_no_match_mat.csv")




## add unit_ids
unsd <- unsd %>% 
  left_join(.,
            source_unit_ids %>% 
              filter(source == "UNSD_Code") %>% 
              select(-source),
            by = c("unit" = "source_unit_id")
  )

# no_match table
  # (including unit names from UNSD)

# read country code table of UNSD
unsd_unit_codes <- read_xlsx(path = "./03_intermediate/01_data_retrieval/unsd/Energy DB Codes 2019.xlsx", 
                            sheet = "Units")

no_match_unit <- unsd %>%
  filter(is.na(unit_id)) %>%
  distinct(unit) %>%
  left_join(.,
            unsd_unit_codes %>%
              select(Code, Unit), 
            by = c("unit" = "Code")
  )

# save csv
write_delim(no_match_unit, file = "./04_output/01_harmonization/01_conc_nomatch/unsd_no_match_unit.csv")






## adjust "Other hydrocarbons"
unsd <- unsd %>%
  mutate(
    material_id = ifelse(
      alphanumiso == "CAN124" & material_id == "F.oth",
      "Ff.ois",
      material_id
    )) %>%
  mutate(
    material_id = ifelse(
      alphanumiso == "VEN862" & material_id == "F.oth",
      "F.cro",
      material_id
    )) %>%
  mutate(
    material_id = ifelse(
      material_id == "F.oth",
      NA,
      material_id
    )) %>%
  filter(!is.na(value)) %>%
  group_by(alphanumiso, material_id, year, unit_id) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()




# remove NAs
unsd <- unsd %>%
  filter(!is.na(alphanumiso)) %>%
  filter(!is.na(material_id)) %>%
  filter(!is.na(unit_id))




# aggregate
unsd <- unsd %>%
  filter(!is.na(value)) %>%
  group_by(alphanumiso, material_id, year, unit_id) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()



# remove obsolete years
unsd <- unsd %>%
  filter(year >= 1970)



# save rds
write_rds(unsd, file = "./03_intermediate/02_data_harmonization/unsd.rds")




