### Yearly sums of China and globally with MFA4 categories





library(tidyverse)
library(openxlsx)
library(scales)

# clear R environment
rm(list = ls())



### read files

## final data

# function to get the last modified file in a directory with a certain pattern in it
source("./02_scripts/00_functions/lastfile.R")

# get the file path of the latest file with a given pattern
lastfile <- my_lastfile("./04_output/02_final/", ".rds")

final <- read_rds(lastfile)

## IDs
mat_ids <- read_delim("./01_input/01_concordance_tables/material_ids.csv", delim = ";")

ass_ore_ids <- read_delim("./01_input/01_concordance_tables/associated_and_dominant_ids.csv", delim = ";") %>%
  rename(
    associated_mat_id = material_id,
    material_category = category
  )

ccc_vs_mat <- read_delim("./01_input/01_concordance_tables/ccc_vs_mat_ids.csv", delim = ";")

mfa13_ids <- read_delim("./01_input/01_concordance_tables/CCC_To_EWMFA_4_and_13_20171010.csv", delim = ";")

cou_ids <- read_delim("./01_input/01_concordance_tables/country_ids.csv", delim = ";")


## lists
cou_for_aft <- read_delim("./01_input/01_concordance_tables/country_former_after.csv", delim = ";")





# yearly sums (for checking)
y_sums <- final %>%
  group_by(year) %>%
  summarise(value = sum(value, na.rm = TRUE))









## China and global (per MFA4) ----

# global
y_sums_mfa4_global <- final %>%
  left_join(.,
            ccc_vs_mat %>%
              select(material_id, MFA4Plus)
  ) %>%
  group_by(year, MFA4Plus) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  pivot_wider(.,
              names_from = MFA4Plus,
              values_from = value
  ) %>%
  left_join(.,
            y_sums %>%
              rename(global = value)
  )


# china
y_sums_mfa4_chn <- final %>%
  filter(alphanumiso == "CHN156") %>%
  left_join(.,
            ccc_vs_mat %>%
              select(material_id, MFA4Plus)
  ) %>%
  group_by(year, MFA4Plus) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  pivot_wider(.,
              names_from = MFA4Plus,
              values_from = value
  ) %>%
  left_join(.,
            final %>%
              filter(alphanumiso == "CHN156") %>%
              group_by(year) %>%
              summarise(value = sum(value, na.rm = TRUE)) %>%
              rename(total = value)
  )



# save tables into Excel file
write.xlsx(
  x = list(
    global = y_sums_mfa4_global, 
    china = y_sums_mfa4_chn
  ),
  file = paste0(
    "./03_intermediate/05_data_check/03_other/china_year_sums_", 
    substr(Sys.time(),1,10), 
    ".xlsx"
  ),
  overwrite = TRUE
)






## get overview of distribution of most recent years
latest_year_distr <- final %>%
  filter(alphanumiso == "CHN156") %>%
  group_by(alphanumiso, material_id) %>%
  summarise(latest_year = max(year, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(latest_year) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(latest_year)) %>%
  mutate(share = percent(n/sum(n)))
