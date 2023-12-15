##### Final cleaning
  ### Especially geographically
    ### i.e. countries which shouldn't have entries in a certain year
    ### i.e. double counting between predecessor and sucessor countries
  ### Steps:
    # Adjust based on years in which a country should exist / have existed
    # Adjust based on possible double entries between predecessor and successor states
    
      




library(tidyverse)


# clear R environment
rm(list = ls())



## read files

# data
data <- read_rds("./03_intermediate/4b_data_estimation/data_with_constr.rds")


# lists
geo_exist <- read_delim("./01_input/01_concordance_tables/geo_exist.csv", delim = ";")

cou_for_aft <- read_delim("./01_input/01_concordance_tables/country_former_after.csv", delim = ";")




## based on years in which a country should exist / have existed
data <- data %>%
  left_join(.,
            geo_exist %>%
              select(-former)
              )

# removing all entries which shouldn't be there based on first and last years of specific countries
data <- data %>%
  mutate(
    value = ifelse(
      !is.na(first_year) & year < first_year,
      NA,
      value
    )
  ) %>%
  mutate(
    value = ifelse(
      !is.na(last_year) & year > last_year,
      NA,
      value
    )
  ) %>%
  filter(!is.na(value)) %>%
  select(-first_year, -last_year)
  





## based on possible double entries between predecessor and successor states
list_merge <- cou_for_aft %>%
  filter(category == "merge") %>%
  select(former, after)


temp1 <- data %>%
  left_join(.,
            list_merge,
            by = c("alphanumiso" = "former")
            ) %>%
  filter(!is.na(after))

temp2 <- data %>%
  left_join(.,
            list_merge,
            by = c("alphanumiso" = "former")
  ) %>%
  filter(is.na(after)) %>%
  select(-after)


# filter for those entries from predecessor states which do not exist for the successor state and re-join with data
temp3 <- setdiff(
  temp1 %>%
    select(after, year, material_id) %>%
    rename(alphanumiso = after),
  temp2 %>%
    select(alphanumiso, year, material_id)
  ) %>%
  distinct(alphanumiso, year, material_id) %>%
  left_join(.,
            list_merge,
            by = c("alphanumiso" = "after")
            ) %>%
  select(-alphanumiso) %>%
  rename(alphanumiso = former) %>%
  left_join(., temp1) %>%
  filter(!is.na(value)) %>%
  select(-after)


# combine both filtered data sets again
data <- temp2 %>%
  union(., temp3)
  







# save rds
write_rds(data, "./03_intermediate/06_final_formatting/final_cleaning.rds")


# 
# data <- read_rds("./03_intermediate/06_final_formatting/adjusted_outliers.rds")
# 
# data1 <- read_rds("./03_intermediate/06_final_formatting/final_cleaning.rds")
# 
# data1[(data1[,1:5] %in% data[,1:3])==F,]
# yy <- setdiff(data[,1:5],data1[,1:5])
# table(yy$source)

