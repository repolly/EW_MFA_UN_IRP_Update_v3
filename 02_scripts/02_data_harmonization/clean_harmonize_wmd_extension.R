##### Clean and Harmonize wmd data extension from 2021
  ### Steps:
    # Filter out rows which have been selected as "too uncertain"
    # Add alphanumiso
      # Check for missing allocations
    # Add material_ids
      # Check for missing allocations
    # Add unit_ids
      # Check for missing allocations
    # Format
    # Aggregate







library(tidyverse)
library(readxl)

# clear R environment
rm(list = ls())



#### clean -----------------

## read files

# data
wmd_ext <- read_delim(
  "./01_input/04_former_data/wmd/WMD_data_extension_202103_corrected.csv", 
  delim = ";", 
  col_types = cols(include = "c")
  )

# IDs
source_cou_ids <- read_delim("./01_input/01_concordance_tables/source_country_ids.csv", delim = ";")

source_mat_ids <- read_delim("./01_input/01_concordance_tables/source_material_ids.csv", delim = ";")

source_unit_ids <- read_delim("./01_input/01_concordance_tables/source_unit_ids.csv", delim = ";")





# placeholder
ph <- wmd_ext
wmd_ext <- ph



# filter out rows which have been selected as "too uncertain" and remove obsolete columns
wmd_ext <- wmd_ext %>%
  filter(is.na(include)) %>%
  select(-source, -comment, -include)



# format columns
wmd_ext <- wmd_ext %>%
  mutate(across(year, as.integer))



### add IDs

## add alphanumiso
wmd_ext <- wmd_ext %>% 
  left_join(
    source_cou_ids %>% 
      filter(source == "WMD_ext") %>% 
      select(-source),
    by = c("country" = "source_country_id")
  )

## no_match table
no_match_cou <- wmd_ext %>%
  filter(is.na(alphanumiso)) %>%
  distinct(country)

# save no_match table as csv
write_delim(no_match_cou, "./04_output/01_harmonization/01_conc_nomatch/wmd_ext_no_match_cou.csv", delim = ";")



## add material_id
wmd_ext <- wmd_ext %>% 
  left_join(
    source_mat_ids %>% 
      filter(source == "WMD_ext") %>% 
      select(-source),
    by = c("commodity" = "source_material_id")
  )

## no_match table
no_match_mat <- wmd_ext %>%
  filter(is.na(material_id)) %>%
  distinct(commodity)

# save no_match table as csv
write_delim(no_match_mat, "./04_output/01_harmonization/01_conc_nomatch/wmd_ext_no_match_mat.csv", delim = ";")



## add unit_id
wmd_ext <- wmd_ext %>% 
  left_join(
    source_unit_ids %>% 
      filter(source == "WMD_ext") %>% 
      select(-source),
    by = c("unit" = "source_unit_id")
  )

## no_match table
no_match_unit <- wmd_ext %>%
  filter(is.na(unit_id)) %>%
  distinct(unit)

# save no_match table as csv
write_delim(no_match_unit, "./04_output/01_harmonization/01_conc_nomatch/wmd_ext_no_match_unit.csv", delim = ";")





# format and remove NAs
wmd_ext <- wmd_ext %>%
  select(alphanumiso, material_id, unit_id, year, value) %>%
  filter(!is.na(alphanumiso)) %>%
  filter(!is.na(material_id)) %>%
  filter(!is.na(unit_id))




# aggregate
wmd_ext <- wmd_ext %>%
  filter(!is.na(value)) %>%
  group_by(alphanumiso, material_id, unit_id, year) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()



# save rds
write_rds(wmd_ext, file = "./03_intermediate/02_data_harmonization/wmd_ext_harmonized.rds")

