######## Lithium data

  ### Save three tables
    ### Final data with names
    ### Harmonized data after integration, before estimation
    ### Harmonized, not integrated, not converted



library(tidyverse)

# clear R environment
rm(list = ls())



### read files


## final data

# function to get the last modified file in a directory with a certain pattern in it
source("./02_scripts/00_functions/lastfile.R")

# get the latest final detailed file
lastfile <- my_lastfile("./04_output/02_final/", "DE_met_min_fos_Detailed_with_names")

final_detailed <- read_delim(lastfile, delim = ";", col_types = cols(unit = "c"))



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







### final detailed data

# filter for material and related materials
data_1 <- final_detailed %>%
  left_join(.,
            mat_ids %>%
              select(material_id, associated_mat_id)
            ) %>%
  filter(material_id %in% c("Me.Li", "AO.Li") | associated_mat_id == "Me.Li") %>%
  select(-associated_mat_id) %>%
  arrange(alphanumiso, year, CCC_Code, material_id)


# # add names and order columns
# data_1 <- data_1 %>%
#   left_join(.,
#             cou_ids %>%
#               select(alphanumiso, country)
#   ) %>%
#   left_join(.,
#             ccc_vs_mat %>%
#               distinct(material_id, material_name)
#   ) %>%
#   left_join(.,
#             mfa13_ids %>%
#               select(CCC_Code, CCC_Name, MFA13)
#   ) %>%
#   select(alphanumiso, CCC_Code, material_id, year, unit, value, source, country, CCC_Name, material_name, MFA13)


# save data as csv
write_delim(
  data_1,
  paste0(
    "./04_output/04_requests/lithium/lithium_final_detailed_",
    substr(Sys.time(),1,10),
    ".csv"
  ), 
  delim = ";", 
  na = "")


# save data only with most essential columns
write_delim(
  data_1 %>%
    select(country, material_name, year, unit, value, source),
  paste0(
    "./04_output/04_requests/lithium/lithium_final_short_",
    substr(Sys.time(),1,10),
    ".csv"
  ), 
  delim = ";", 
  na = "")






### converted-integrated

# filter for material and related materials
data_2 <- converted %>%
  left_join(.,
            mat_ids %>%
              select(material_id, associated_mat_id)
  ) %>%
  filter(material_id %in% c("Me.Li", "AO.Li") | associated_mat_id == "Me.Li") %>%
  select(-associated_mat_id) %>%
  arrange(alphanumiso, year, material_id)


# save data as csv
write_delim(
  data_2,
  paste0(
    "./04_output/04_requests/lithium/lithium_interm_conv_integr_",
    substr(Sys.time(),1,10),
    ".csv"
  ), 
  delim = ";", 
  na = "")






### harmonized

# put harmonized tables together and filter for material and related materials
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
  left_join(.,
            mat_ids %>%
              select(material_id, associated_mat_id)
  ) %>%
  filter(material_id %in% c("Me.Li", "AO.Li") | associated_mat_id == "Me.Li") %>%
  select(-associated_mat_id) %>%
  arrange(alphanumiso, year, material_id)


# save data as csv
write_delim(
  data_3,
  paste0(
    "./04_output/04_requests/lithium/lithium_harmonized_all_",
    substr(Sys.time(),1,10),
    ".csv"
  ), 
  delim = ";", 
  na = "")







