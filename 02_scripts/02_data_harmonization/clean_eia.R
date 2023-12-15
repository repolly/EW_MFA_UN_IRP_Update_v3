##### Cleaning/Formatting of data retrieved from EIA API
  ### Steps:
    # Rename and format column types
    # Split commodity-country info
    # Delete obsolete columns




library(tidyverse)

# clear R environment
rm(list = ls())




## read files

# data
source("./02_scripts/00_functions/lastfile.R")

eia <- my_lastfile("./03_intermediate/01_data_retrieval/eia/", "eia_dldate") %>%
  read_rds()




## format

# rename and format columns
eia <- eia %>%
  rename(year = X1, value = X2) %>%
  mutate(value = ifelse(value %in% c("--", "NA"), NA, value)) %>%
  mutate(
    across(year, as.integer),
    across(value, as.numeric)
    )


# split info on commodity and country (which comes combined)
eia <- eia %>% 
  tidyr::separate(
    col = name,
    into = "commodity", 
    sep = ",", 
    extra = "drop"
    )



# remove obsolete columns
eia <- eia %>%
  select(-series_id, -start, -end)



# save rds
write_rds(eia, file = "./03_intermediate/02_data_harmonization/clean_eia.rds")

