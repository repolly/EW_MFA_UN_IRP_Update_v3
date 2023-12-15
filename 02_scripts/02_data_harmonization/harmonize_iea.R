##### Harmonize data from IEA
  ### Steps:
    # Harmonize recent data: Last ten years
      # Add alphanumiso
        # Check for missing allocations
      # Add material_ids
        # Check for missing allocations
      # Add unit_ids
        # Check for missing allocations
      # Aggregate
    # Adjust "Other hydrocarbons"
    # Adjust special cases






library(tidyverse)
library(readxl)

# clear R environment
rm(list = ls())



### read files

## data

# recent data
source("./02_scripts/00_functions/lastfile.R")

lastfile <- my_lastfile("./03_intermediate/01_data_retrieval/iea/", "WBES_")

iea <- read_delim(
  lastfile,
  delim = ",",
  col_names  = TRUE,
  col_types = cols(Time = "i", Value = "n"),
  locale = locale(encoding = "UTF-8")
  )


# # historical data
# iea_hist <- read_delim(
#   "./01_input/04_former_data/iea/iea_coal_disagg_values.csv",
#   delim = ";"
# )
# 
# # correct encoding for variable "Country2" (e.g. because of cases like "Côte d'Ivoire" and "Curaçao")
# Encoding(iea_hist$Country2) <- "UTF-16"


# concordance tables
source_cou_ids <- read_delim("./01_input/01_concordance_tables/source_country_ids.csv", delim = ";")

source_mat_ids <- read_delim("./01_input/01_concordance_tables/source_material_ids.csv", delim = ";")

source_unit_ids <- read_delim("./01_input/01_concordance_tables/source_unit_ids.csv", delim = ";")


# load special cases
spec_case <- read_delim("./01_input/02_log_files/iea/special_cases.csv", ";")








### harmonization of recent data ----------------

## format

# filter for "Production"
iea <- iea %>% 
  filter(Flow == "Production")


# delete obsolete columns and rename those which are kept later
iea <- iea %>% 
  select(-c("PRODUCT", "FLOW", "Flow", "TIME", "Flag Codes", "Flags")) %>%
  rename("year" = "Time", "value" = "Value")





### add IDs

## add country_ids
iea <- iea %>% 
  left_join(.,
            source_cou_ids %>% 
              filter(source == "IEA_Code") %>% 
              select(-source),
            by = c("COUNTRY" = "source_country_id")
  )

## no_match table
no_match_cou <- iea %>%
  filter(is.na(alphanumiso)) %>%
  distinct(COUNTRY, Country)

# save csv
write_delim(no_match_cou, file = "./04_output/01_harmonization/01_conc_nomatch/iea_no_match_cou.csv")





## add mat_ids
iea <- iea %>% 
  left_join(.,
            source_mat_ids %>% 
              filter(source == "IEA") %>% 
              select(-source),
            by = c("Product" = "source_material_id")
  )

## no_match table
no_match_mat <- iea %>%
  filter(is.na(material_id)) %>%
  distinct(Product)


# save csv
write_delim(no_match_mat, file = "./04_output/01_harmonization/01_conc_nomatch/iea_no_match_mat.csv")




## add unit_ids
  ## based on product names, which contain the units
iea <- iea %>%
  mutate(
    unit_id = case_when(
      grepl("kt", Product) ~ "Kt",
      grepl("TJ-gross", Product) ~ "TJ"
      )
    )

## no_match table
  ## here the output contains the IEA product, because that's the variable which contains the unit
no_match_unit <- iea %>%
  filter(!is.na(alphanumiso)) %>%
  filter(!is.na(material_id)) %>%
  filter(is.na(unit_id)) %>%
  distinct(Product)

# save csv
write_delim(no_match_unit, file = "./04_output/01_harmonization/01_conc_nomatch/iea_no_match_unit.csv")





# remove NAs
iea <- iea %>%
  filter(!is.na(alphanumiso)) %>%
  filter(!is.na(material_id)) %>%
  filter(!is.na(unit_id))



# aggregate
iea <- iea %>%
  filter(!is.na(value)) %>%
  group_by(alphanumiso, material_id, year, unit_id) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()





## adjust "Other hydrocarbons"
iea <- iea %>%
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
iea <- iea %>%
  filter(!is.na(alphanumiso)) %>%
  filter(!is.na(material_id)) %>%
  filter(!is.na(unit_id))




## adjust special cases
iea <- iea %>%
  left_join(.,
            spec_case %>%
              select(-comment)
  ) %>%
  mutate(
    alphanumiso = ifelse(
      !is.na(alphanumiso_new) & year >= year_start & year <= year_end, 
      alphanumiso_new, 
      alphanumiso
    )) %>%
  select(-alphanumiso_new, -year_start, -year_end)





# save rds
write_rds(iea, file = "./03_intermediate/02_data_harmonization/iea.rds")


