######## Save intermediate data as csv

### Without filtering for specific materials

### Harmonized data after integration, before estimation
### Harmonized, not integrated, not converted



library(tidyverse)

# clear R environment
rm(list = ls())



### read files


## data after integration and unit conversion
converted <- read_rds("./03_intermediate/04a_data_conversion/conversion_units/data_converted_new.rds")



# harmonized data sets (before everything else)
wmd <- read_rds("./03_intermediate/02_data_harmonization/wmd_comb.rds")
bgs <- read_rds("./03_intermediate/02_data_harmonization/bgs_harmonized.rds")
usgs <- read_rds("./03_intermediate/02_data_harmonization/usgs_comb_plus_ext.rds")
wmd_ext <- read_rds("./03_intermediate/02_data_harmonization/wmd_ext_harmonized.rds")



# IDs and factors
mat_ids <- read_delim("./01_input/01_concordance_tables/material_ids.csv", ";")

ccc_vs_mat <- read_delim("./01_input/01_concordance_tables/ccc_vs_mat_ids.csv", delim = ";")

mfa13_ids <- read_delim("./01_input/01_concordance_tables/CCC_To_EWMFA_4_and_13_20171010.csv", delim = ";")

cou_ids <- read_delim("./01_input/01_concordance_tables/country_ids.csv", delim = ";")







### converted-integrated

# filter for material and related materials
data_2 <- converted %>%
  arrange(alphanumiso, year, material_id)


# save data as csv
write_delim(
  data_2,
  paste0(
    "./04_output/04_requests/all_data/all_interm_conv_integr_",
    substr(Sys.time(),1,10),
    ".csv"
  ), 
  delim = ";", 
  na = "")






### harmonized

# put harmonized tables together
  # here only for wmd, bgs, usgs
data_3 <- wmd %>%
  select(-rep) %>%
  mutate(source = "wmd") %>%
  union(.,
        bgs %>%
          select(-rep) %>%
          mutate(source = "bgs")
  ) %>%
  union(.,
        usgs %>%
          mutate(source = "usgs")
  ) %>%
  union(.,
        wmd_ext %>%
          mutate(source = "wmd_ext")
  ) %>%
  arrange(alphanumiso, year, material_id)


# save data as csv
write_delim(
  data_3,
  paste0(
    "./04_output/04_requests/all_data/wmd_bgs_usgs_interm_harmonized_",
    substr(Sys.time(),1,10),
    ".csv"
  ), 
  delim = ";", 
  na = "")







