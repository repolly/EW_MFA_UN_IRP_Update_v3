### Format and harmonize old estimation factors (i.e. from former database)
  ### for gaps of new estimation factors



library(tidyverse)


# clear R environment
rm(list = ls())



## read files

# data
factors <- read_delim("./01_input/04_former_data/former_db/t_factor_formatted.csv", delim = ";")

# IDs
cou_ids <- read_delim("./01_input/01_concordance_tables/country_ids.csv", delim = ";")

mat_ids <- read_delim("./01_input/01_concordance_tables/material_ids.csv", delim = ";")





# select variables and filter
factors <- factors %>%
  select(CountryID, CommodityID, Concentration, UsedSource) %>%
  mutate(across(CommodityID, as.double)) %>%
  filter(!(CommodityID %in% c(22, 42)))



# include mat_ids and filter
factors <- factors %>%
  left_join(.,
            mat_ids %>%
              filter(material_category == "commodity") %>%
              select(material_id, formerly_associated_wu_id),
            by = c("CommodityID" = "formerly_associated_wu_id")
  ) %>%
  filter(!is.na(material_id)) %>%
  filter(!(Concentration %in% c(0, 1))) %>%
  filter(!is.na(Concentration))



# include cou_ids and filter
factors <- factors %>%
  left_join(.,
            cou_ids %>%
              select(alphanumiso, wu_former_id),
            by = c("CountryID" = "wu_former_id")
  ) %>%
  filter(!is.na(alphanumiso))



# save as csv
write_delim(factors, "./01_input/04_former_data/former_db/old_factors.csv", delim = ";")





