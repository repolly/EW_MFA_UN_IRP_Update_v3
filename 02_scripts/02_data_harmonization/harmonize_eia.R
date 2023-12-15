##### Harmonize data from EIA
  ### Steps:
    # Add alphanumiso
      # Check for missing allocations
    # Add material_ids
      # Check for missing allocations
    # Add unit_ids
      # Check for missing allocations
    # Aggregate






library(tidyverse)

# clear R environment
rm(list = ls())



## read files

# data
eia <- read_rds("./03_intermediate/02_data_harmonization/clean_eia.rds")


# concordance tables
source_cou_ids <- read_delim("./01_input/01_concordance_tables/source_country_ids.csv", delim = ";")

source_mat_ids <- read_delim("./01_input/01_concordance_tables/source_material_ids.csv", delim = ";")

source_unit_ids <- read_delim("./01_input/01_concordance_tables/source_unit_ids.csv", delim = ";")





### add IDs

## add country_ids
eia <- eia %>% 
  left_join(.,
            source_cou_ids %>% 
              filter(source == "EIA_Code") %>% 
              select(-source),
            by = c("geography" = "source_country_id")
  )

## no_match table
no_match_cou <- eia %>%
  filter(is.na(alphanumiso)) %>%
  distinct(geography)

# save csv
write_delim(no_match_cou, file = "./04_output/01_harmonization/01_conc_nomatch/eia_no_match_cou.csv")





## add mat_ids
eia <- eia %>% 
  left_join(.,
            source_mat_ids %>% 
              filter(source == "EIA") %>% 
              select(-source),
            by = c("commodity" = "source_material_id")
  )

## no_match table
no_match_mat <- eia %>%
  filter(is.na(material_id)) %>%
  distinct(commodity)


# save csv
write_delim(no_match_mat, file = "./04_output/01_harmonization/01_conc_nomatch/eia_no_match_mat.csv")




## add unit_ids
eia <- eia %>% 
  left_join(.,
            source_unit_ids %>% 
              filter(source == "EIA") %>% 
              select(-source),
            by = c("units" = "source_unit_id")
  )

# no_match table
no_match_unit <- eia %>%
  filter(is.na(unit_id)) %>%
  distinct(units)

# save csv
write_delim(no_match_unit, file = "./04_output/01_harmonization/01_conc_nomatch/eia_no_match_unit.csv")





# remove NAs
eia <- eia %>%
  filter(!is.na(alphanumiso)) %>%
  filter(!is.na(material_id)) %>%
  filter(!is.na(unit_id))



# aggregate
eia <- eia %>%
  filter(!is.na(value)) %>%
  group_by(alphanumiso, material_id, year, unit_id) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()



# save rds
write_rds(eia, file = "./03_intermediate/02_data_harmonization/eia.rds")









