### detailed comparison 2017 vs 2021

  # should be archived
  # results are not really comparable -> too many special considerations necessary


library(tidyverse)

# clear R environment
rm(list = ls())



### read files ----

## final data

# function to get the last modified file in a directory with a certain pattern in it
source("./02_scripts/00_functions/lastfile.R")

# get the file path of the latest file with a given pattern
lastfile <- my_lastfile("./04_output/02_final/", ".rds")

current <- read_rds(lastfile)

former <- read_delim("./01_input/04_former_data/former_irp_database/MFA_met_min_fos_detailed_table_20170501_rev.csv", delim = ",")


## IDs
mat_ids <- read_delim("./01_input/01_concordance_tables/material_ids.csv", delim = ";")





## format

# former
colnames(former) <- c("alphanumiso", "CCC_Code", "CommodityID", "year", "value")

former <- former %>%
  mutate(value = value * 1000) %>%
  mutate(
    alphanumiso = ifelse(
      alphanumiso == "KOSOVO",
      "UNK000",
      alphanumiso
    ))



# include material_id in current data
current <- current %>%
  left_join(.,
            mat_ids %>%
              select(material_id, formerly_associated_wu_id)) %>%
  rename(CommodityID = formerly_associated_wu_id)


# aggregate current data
current_agg <- current %>%
  group_by(alphanumiso, CommodityID, year) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(., 
            former %>%
              distinct(CCC_Code, CommodityID) %>%
              filter(!is.na(CommodityID))
            )


# combine
combined <- current_agg %>%
  full_join(.,
            former,
            by = c("alphanumiso", "CommodityID", "year", "CCC_Code"),
            suffix = c("_curr", "_form")
            )


# remove anything which can't fit together
  # e.g. due to fossil fuels in former data not carrying a CommodityID
combined <- combined %>%
  filter(!grepl("A.4", CCC_Code))


# compare
combined <- combined %>%
  mutate(
    diff = case_when(
      !is.na(value_curr) & !is.na(value_form) ~ value_curr - value_form,
      !is.na(value_curr) & is.na(value_form) ~ value_curr - 0,
      is.na(value_curr) & !is.na(value_form) ~ 0 - value_form
    )
  ) %>%
  arrange(desc(diff)) %>%
  filter(!is.na(CCC_Code)) %>%
  filter(diff != 0)




