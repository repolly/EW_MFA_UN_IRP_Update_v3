##### Final checks
  ### Steps:
    # Check double counting
      # by material_category and associated_id
      # by list of former-after countries
      # for countries which split
    # Check highest numbers
    # Comparison to former version
      # share of number of data points changed per ccc
      # share of number of data points changed per year
      # share of number of data points changed per country
      # absolute change by year
      # largest positive changes per year and ccc
      # largest negative changes per year and ccc
      # number of data points changed (positive, negative, new/removed or changed materials)
    # Save comparison files






library(tidyverse)
library(openxlsx)
library(scales)

# clear R environment
rm(list = ls())



### read files ----

## final data

# function to get the last modified file in a directory with a certain pattern in it
source("./02_scripts/00_functions/lastfile.R")

# get the file path of the latest file with a given pattern
lastfile <- my_lastfile("./04_output/02_final/", ".rds")

final <- read_rds(lastfile)

former <- read_rds("./04_output/02_final/DE_met_min_fos_Detailed_2022-11-23.rds")


## IDs
mat_ids <- read_delim("./01_input/01_concordance_tables/material_ids.csv", delim = ";")

ass_ore_ids <- read_delim("./01_input/01_concordance_tables/associated_and_dominant_ids.csv", delim = ";") %>%
  rename(
    associated_mat_id = material_id,
    material_category = category
    )

ccc_vs_mat <- read_delim("./01_input/01_concordance_tables/ccc_vs_mat_ids.csv", delim = ";")

ccc <- read_delim("./01_input/01_concordance_tables/ccc.csv", delim = ";")

mfa13_ids <- read_delim("./01_input/01_concordance_tables/CCC_To_EWMFA_4_and_13_20171010.csv", delim = ";")

cou_ids <- read_delim("./01_input/01_concordance_tables/country_ids.csv", delim = ";")


## lists
cou_for_aft <- read_delim("./01_input/01_concordance_tables/country_former_after.csv", delim = ";")




# # temp filter
# final <- final %>%
#   filter(!material_id %in% c("O.dii", "O.dig", "O.dia"))



# yearly sums (for checking)
y_sums <- final %>%
  group_by(year) %>%
  summarise(value = sum(value, na.rm = TRUE))







### check double counting ----

## by material_category and associated_id
  ## i.e. there should in most cases only be one material category per country and year and material

# list of mat_ids and associated_mat_ids and material_categories
  # including associated ores (i.e. "AO.[]")
  # including each associated_id as its own associated_id (in order to proper check for doubles)
ass_ids <- mat_ids %>%
  distinct(material_id, associated_mat_id, material_category) %>%
  filter(!is.na(material_id)) %>%
  filter(!is.na(associated_mat_id)) %>%
  filter(!is.na(material_category)) %>%
  union(.,
        ass_ore_ids %>%
          filter(material_category == "associated") %>%
          rename(material_id = ore_id) %>%
          select(-associated_ore_name)
        ) %>%
  union(.,
        tibble(
          material_category = "associated_mat_ids",
          material_id = mat_ids %>%
            distinct(associated_mat_id) %>%
            filter(!is.na(associated_mat_id)) %>%
            pull()
          ) %>%
          mutate(associated_mat_id = material_id)
        )



doub_mat <- final %>%
  left_join(., ass_ids) %>%
  distinct(alphanumiso, year, associated_mat_id, material_category) %>%
  filter(!is.na(associated_mat_id)) %>%
  group_by(alphanumiso, year, associated_mat_id) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n > 1)

doub_mat %>% distinct(associated_mat_id)

overview_mat <- doub_mat %>%
  left_join(., ass_ids) %>%
  left_join(., final) %>%
  filter(!is.na(value)) %>%
  pivot_wider(.,
              names_from = year,
              values_from = value)










## by list of former-after countries

# for countries which merged

list_merge_1 <- cou_for_aft %>%
  filter(category == "merge") %>%
  select(former, after) %>%
  rename(alphanumiso = former)

list_merge_2 <- cou_for_aft %>%
  filter(category == "merge") %>%
  select(former, after) %>%
  rename(alphanumiso = after)


doub_cou_cov_m <- final %>%
  left_join(., list_merge_1) %>%
  filter(!is.na(after)) %>%
  distinct(after, year, material_id) %>%
  rename(alphanumiso = after)


doub_cou_merge_1 <- intersect(
  doub_cou_cov_m,
  final %>%
    distinct(alphanumiso, year, material_id)
)


doub_cou_merge_2 <- doub_cou_merge_1 %>%
  left_join(., final) %>%
  mutate(after = alphanumiso) %>%
  union(.,
        doub_cou_merge_1 %>%
          left_join(., list_merge_2) %>%
          rename(after = alphanumiso) %>%
          rename(alphanumiso = former) %>%
          left_join(., final) %>%
          filter(!is.na(value))
        )


overv_cou_merge <- doub_cou_merge_2 %>%
  arrange(year) %>%
  pivot_wider(.,
              names_from = year,
              values_from = value
              ) %>%
  arrange(material_id, after)









# for countries which split

list_split_1 <- cou_for_aft %>%
  filter(category == "split") %>%
  select(former, after) %>%
  rename(alphanumiso = after)

list_split_2 <- cou_for_aft %>%
  filter(category == "split") %>%
  select(former, after) %>%
  rename(alphanumiso = former)


doub_cou_cov_s <- final %>%
  left_join(., list_split_1) %>%
  filter(!is.na(former)) %>%
  distinct(former, year, material_id) %>%
  rename(alphanumiso = former)


doub_cou_split_1 <- intersect(
  doub_cou_cov_s,
  final %>%
    distinct(alphanumiso, year, material_id)
)


doub_cou_split_2 <- doub_cou_split_1 %>%
  left_join(., final) %>%
  mutate(former = alphanumiso) %>%
  union(.,
        doub_cou_split_1 %>%
          left_join(., list_split_2) %>%
          rename(former = alphanumiso) %>%
          rename(alphanumiso = after) %>%
          left_join(., final) %>%
          filter(!is.na(value))
  )


overv_cou_split <- doub_cou_split_2 %>%
  arrange(year) %>%
  pivot_wider(.,
              names_from = year,
              values_from = value
  ) %>%
  arrange(material_id, former)









### check highest numbers ----

highest_val <- final %>%
  arrange(desc(value))

highest_val <- highest_val[1:1000,]








### comparison to former version ----

# for external provision of info on highest changes
comp_former <- final %>%
  full_join(.,
            former,
            by = c("CCC_Code", "alphanumiso", "material_id", "year", "unit"),
            suffix = c("_curr", "_form")
  ) %>%
  select(CCC_Code, alphanumiso, material_id, year, unit, value_curr, value_form) %>%
  mutate(
    diff = case_when(
      !is.na(value_curr) & !is.na(value_form) ~ value_curr - value_form,
      !is.na(value_curr) & is.na(value_form) ~ value_curr - 0,
      is.na(value_curr) & !is.na(value_form) ~ 0 - value_form
    )
  ) %>%
  arrange(CCC_Code, desc(diff))


# for own comparison of highest values and highest changes
comp_former_2 <- comp_former %>%
  arrange(desc(diff))



# include names, adjust names, adjust order
comp_former_names <- comp_former %>%
  left_join(., ccc) %>%
  select(-MFA4Plus) %>%
  left_join(.,
            ccc_vs_mat %>%
              select(material_id, material_name)
            ) %>%
  left_join(.,
            cou_ids %>%
              select(alphanumiso, country)
            ) %>%
  rename(
    value_current = value_curr,
    value_former = value_form,
    difference = diff
  ) %>%
  filter(!is.na(country)) %>%
  select(CCC_Code, alphanumiso, material_id, year, value_current, value_former, difference, CCC_Name, country, material_name, unit, MFA13Plus)





# share of number of data points changed per ccc
rel_change_data_points_ccc <- comp_former_names %>%
  filter(difference != 0) %>%
  group_by(CCC_Code, CCC_Name) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(total = sum(n, na.rm = TRUE)) %>%
  mutate(share = n / total) %>%
  filter(share != 0) %>%
  arrange(desc(share)) %>%
  mutate(share = percent(share, accuracy = 0.01)) %>%
  select(-total)

# share of number of data points changed per year
rel_change_data_points_year <- comp_former_names %>%
  filter(difference != 0) %>%
  group_by(year) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(total = sum(n, na.rm = TRUE)) %>%
  mutate(share = n / total) %>%
  filter(share != 0) %>%
  arrange(desc(year)) %>%
  mutate(share = percent(share, accuracy = 0.01)) %>%
  select(-total)

# share of number of data points changed per country
rel_change_data_points_country <- comp_former_names %>%
  filter(difference != 0) %>%
  group_by(alphanumiso, country) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(total = sum(n, na.rm = TRUE)) %>%
  mutate(share = n / total) %>%
  filter(share != 0) %>%
  arrange(desc(share)) %>%
  mutate(share = percent(share, accuracy = 0.01)) %>%
  select(-total)


# absolute change by year
abs_change_year <- comp_former_names %>%
  group_by(year) %>%
  summarise(
    value_current = sum(value_current, na.rm = TRUE),
    value_former = sum(value_former, na.rm = TRUE),
    difference = sum(difference, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(desc(year))


# largest positive changes per year and ccc
rel_change_ccc_year_pos <- comp_former_names %>%
  group_by(year, CCC_Code, CCC_Name) %>%
  summarise(difference = sum(difference, na.rm = TRUE)) %>%
  filter(difference > 0) %>%
  group_by(year) %>%
  mutate(total_annual_diff = sum(difference, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(share = difference / total_annual_diff) %>%
  filter(share >= 0.05) %>%
  arrange(desc(year), desc(share)) %>%
  mutate(share = percent(share, accuracy = 0.01)) %>%
  select(-difference, -total_annual_diff)

# largest negative changes per year and ccc
rel_change_ccc_year_neg <- comp_former_names %>%
  group_by(year, CCC_Code, CCC_Name) %>%
  summarise(difference = sum(difference, na.rm = TRUE)) %>%
  filter(difference < 0) %>%
  group_by(year) %>%
  mutate(total_annual_diff = sum(difference, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(share = difference / total_annual_diff) %>%
  mutate(share = share * (-1) ) %>%
  filter(share <= -0.05) %>%
  arrange(desc(year), share) %>%
  mutate(share = percent(share, accuracy = 0.01)) %>%
  select(-difference, -total_annual_diff)


# number of data points changed (positive, negative, new/removed or changed materials)
comp_former_names %>%
  filter(difference > 0)

comp_former_names %>%
  filter(difference < 0)

comp_former_names %>%
  filter(difference != 0) %>%
  filter(is.na(value_former) & !is.na(value_current))

comp_former_names %>%
  filter(difference != 0) %>%
  filter(!is.na(value_former) & is.na(value_current))




## save comparison files
write_delim(
  rel_change_data_points_ccc,
  "./03_intermediate/05_data_check/02_final_checks/comparison_former/rel_change_data_points_ccc.csv",
  delim = ";"
  )

write_delim(
  rel_change_data_points_year,
  "./03_intermediate/05_data_check/02_final_checks/comparison_former/rel_change_data_points_year.csv",
  delim = ";"
)

write_delim(
  rel_change_data_points_country,
  "./03_intermediate/05_data_check/02_final_checks/comparison_former/rel_change_data_points_country.csv",
  delim = ";"
)

write_delim(
  abs_change_year,
  "./03_intermediate/05_data_check/02_final_checks/comparison_former/abs_change_year.csv",
  delim = ";"
)

write_delim(
  rel_change_ccc_year_pos,
  "./03_intermediate/05_data_check/02_final_checks/comparison_former/rel_change_ccc_year_pos.csv",
  delim = ";"
)

write_delim(
  rel_change_ccc_year_neg,
  "./03_intermediate/05_data_check/02_final_checks/comparison_former/rel_change_ccc_year_neg.csv",
  delim = ";"
)

write_delim(
  comp_former_names %>%
    filter(difference != 0),
  "./03_intermediate/05_data_check/02_final_checks/comparison_former/all_changed_values.csv",
  delim = ";",
  na = ""
)


