### comparison CCC 2017 vs 2021

  # for 2017 there are two version:
    # former: official one
      # includes estimations for construction minerals, but not the latest data version from wu
    
    # former_2: wu compilation output (which apparently was not the last version integrated into official verison)
      # does not include estimations on construction minerals, but latest data version from wu


library(tidyverse)

# clear R environment
rm(list = ls())



### read files ----

# ccc
current <- read_delim(
  "./04_output/02_final/DE_met_min_fos_CCC_2021-11-04.csv",
  delim = ";",
  col_types = cols(unit = "c")
)

former <- read_delim("./01_input/04_former_data/former_irp_database/policyDB_CCC_20171122.csv", delim = ",")

former_2 <- read_delim("./01_input/04_former_data/former_irp_database/MFA_met_min_fos_CCC_table_20170501_rev.csv", delim = ",")

mfa13_ids <- read_delim("./01_input/01_concordance_tables/CCC_To_EWMFA_4_and_13_20171010.csv", delim = ";")





## format and correct

# current
current <- current %>%
  filter(year <= 2017) %>%
  select(-unit)


# former
col_names <- as.character(1970:2017)

former <- former %>%
  pivot_longer(.,
               cols = !!col_names,
               names_to = "year",
               values_to = "value"
  ) %>%
  rename(alphanumiso = AlphaNumISO) %>%
  select(-ISOAlpha3, -Country) %>%
  mutate(year = as.integer(year))



# combine
combined <- current %>% 
full_join(.,
          former,
          by = c("alphanumiso", "CCC_Code", "year"),
          suffix = c("_curr", "_form")
          )

# remove obsolete rows
  # with NA in both
  # biomass
combined <- combined %>%
  filter(!(is.na(value_curr) & is.na(value_form))) %>%
  filter(!grepl("A.1", CCC_Code))



# compare
compare <- combined %>%
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


# include CCC names
compare <- compare %>%
  left_join(.,
            mfa13_ids %>%
              select(CCC_Code, CCC_Name)
            )



# filter for fossil fuels
  # (as severe differences have already become known here)
fossils <- compare %>%
  filter(grepl("A.4", CCC_Code))

fossils_2 <- fossils %>%
  filter(is.na(value_curr))



