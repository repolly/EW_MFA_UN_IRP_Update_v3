##### Clean and Harmonize USGS manual data extensions from 2017 and 2021
##### And integrate with each other and with other usgs data
  ### Steps:
    # Harmonize extension 2021
      # Format
      # Check for wrong or missing alphanumiso
      # Check for wrong or missing material_id
      # Check for wrong or missing unit_id
    # Get only the entries different to input data
    # Combine
    # Aggregate






library(tidyverse)
library(readxl)

# clear R environment
rm(list = ls())




## read files

# extension data sets and there respective input files

usgs_ext_2 <- read_xlsx("./03_intermediate/01_data_retrieval/usgs/extension/extension_2022-10-27.xlsx")

usgs_ext_2_input <- read_delim("./03_intermediate/01_data_retrieval/usgs/extension/all_sources_usgs_time_series.csv", delim = ";") %>%
mutate(across(year, as.integer))


# retrieved usgs data + usgs data from former_db
usgs_comb <- read_rds("./03_intermediate/02_data_harmonization/usgs_comb.rds")
#unique(usgs_comb$year)

# IDs
source_cou_ids <- read_delim("./01_input/01_concordance_tables/source_country_ids.csv", delim = ";")

source_mat_ids <- read_delim("./01_input/01_concordance_tables/source_material_ids.csv", delim = ";")

source_unit_ids <- read_delim("./01_input/01_concordance_tables/source_unit_ids.csv", delim = ";")

cou_ids <- read_delim("./01_input/01_concordance_tables/country_ids.csv", delim = ";")

mat_ids <- read_delim("./01_input/01_concordance_tables/material_ids.csv", delim = ";")

unit_ids <- read_delim("./01_input/01_concordance_tables/unit_ids.csv", delim = ";")








#### clean -----------------

### usgs_ext_2 (2021/current) ----

# remove obsolete columns
usgs_ext_2 <- usgs_ext_2 %>%
  select(-country, -material_name, -unit, -source_all, -comment)



# long format
col_names <- names(usgs_ext_2)[!(names(usgs_ext_2) %in% c("alphanumiso", "material_id", "unit_id", "new_entry"))]

usgs_ext_2 <- usgs_ext_2 %>%
  pivot_longer(.,
               cols = !!col_names,
               names_to = "year",
               values_to = "value"
  ) %>%
  mutate(
    year = as.integer(year)
  )



# check for wrong or missing alphanumiso
no_match_cou <- usgs_ext_2 %>% 
  filter(!(alphanumiso %in% cou_ids$alphanumiso)) %>% 
  distinct(alphanumiso) %>%
  union(., usgs_ext_2 %>% 
          filter(is.na(alphanumiso)) %>%
          select(alphanumiso))

# save no_match table as csv
write_delim(no_match_cou, "./04_output/01_harmonization/01_conc_nomatch/usgs_ext_2_no_match_cou.csv", delim = ";")


# check for wrong or missing material_id
no_match_mat <- usgs_ext_2 %>% 
  filter(!(material_id %in% c(mat_ids$material_id, mat_ids$material_id_agg))) %>% 
  distinct(material_id) %>%
  union(., usgs_ext_2 %>% 
          filter(is.na(material_id)) %>%
          select(material_id))

# save no_match table as csv
write_delim(no_match_mat, "./04_output/01_harmonization/01_conc_nomatch/usgs_ext_2_no_match_mat.csv", delim = ";")


# check for wrong or missing unit_id
no_match_unit <- usgs_ext_2 %>% 
  filter(!(unit_id %in% unit_ids$unit_id)) %>% 
  distinct(unit_id) %>%
  union(., usgs_ext_2 %>% 
          filter(is.na(unit_id)) %>%
          select(unit_id))

# save no_match table as csv
write_delim(no_match_unit, "./04_output/01_harmonization/01_conc_nomatch/usgs_ext_2_no_match_unit.csv", delim = ";")






# format and remove NAs
  # here also values which are NA, as it is data collected by WU team
usgs_ext_2 <- usgs_ext_2 %>%
  filter(!is.na(alphanumiso)) %>%
  filter(!is.na(material_id)) %>%
  filter(!is.na(unit_id)) %>%
  filter(!is.na(value))



# filter for only new entries
usgs_ext_2_fil <- usgs_ext_2 %>%
  filter(new_entry == "x")


# get only the entries different to input data
  # avoiding human error for specifying new entries wrong
temp <- setdiff(
  usgs_ext_2_fil %>%
    select(alphanumiso, material_id, year),
  usgs_ext_2_input %>%
    select(alphanumiso, material_id, year)
  )


# re-combine with all respective materials from ext_2 and aggregate
  # in order to not lose any entries with the same mat_id (but not specified as new)
    # which would get lost during integration with other data, because values for the same alphanumiso-mat_id-year are omitted there
usgs_ext_2 <- temp %>%
  left_join(., usgs_ext_2) %>%
  select(-new_entry) %>%
  group_by(alphanumiso, material_id, year, unit_id) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()
  


# only use values and revisions back to 2013, because some values are included which shouldn't be included
usgs_ext_2 <- usgs_ext_2 %>%
  filter(year >= 2013)



# temp filter
  # because wrong values might have slipped into input dataset for ext_2
usgs_ext_2 <- usgs_ext_2 %>%
  filter(!material_id %in% c("O.dii", "O.dig", "O.dia")) %>%
  filter(!material_id %in% c("Nm.dii", "Nm.dig", "Nm.dia"))  






### combine data ----
usgs <- usgs_comb 

# combine ext_2 with the above combination
  # here only with distinct alphanumiso, mat_id, year, in order to avoid double counting due to revisions
  # giving priority to ext_2, because of it containing revisions (and all of the other data already, in optimal case)

# take only from usgs table what is not in ext_2
temp <- setdiff(
  usgs %>%
    distinct(alphanumiso, material_id, year),
  usgs_ext_2 %>%
    distinct(alphanumiso, material_id, year)
  ) %>%
  left_join(., usgs)

# combine
  # with bind_rows, in order to keep entries which have same value for same mat_id
usgs <- usgs_ext_2 %>%
  bind_rows(., temp)






# aggregate
usgs <- usgs %>%
  filter(!is.na(value)) %>%
  group_by(alphanumiso, material_id, unit_id, year) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()



# save rds
write_rds(usgs, file = "./03_intermediate/02_data_harmonization/usgs_comb_plus_ext.rds")

