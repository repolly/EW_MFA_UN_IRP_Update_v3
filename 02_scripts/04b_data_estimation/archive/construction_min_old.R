
library(tidyverse)
library(readxl)


path <- "./01_input/04_former_data/constr_min/AM - Aggregate consumption report ver 1.14.xlsx"

constr <- path %>%
  excel_sheets() %>%
  set_names() %>% 
  map(
    ~ read_excel(
      path = path, 
      sheet = .x, 
      na = c("", "-", "NA")), 
    .id = "sheet"
  )



gravel <- constr[["Gravel"]] %>% mutate(material_id = "M.sag")
sand <- constr[["Sand"]] %>% mutate(material_id = "M.sag")
clay <- constr[["Clay"]] %>% mutate(material_id = "M.coc")
limestone <- constr[["Limestone"]] %>% mutate(material_id = "M.lim")
gypsum <- constr[["Gypsum"]] %>% mutate(material_id = "M.gya")


constr_min <- gravel %>%
  union(., sand) %>%
  union(., clay) %>%
  union(., limestone) %>%
  union(., gypsum)


col_names <- c(1950:1969) %>% as.character()

constr_min <- constr_min %>%
  select(-!!col_names)


col_names <- c(1970:2011) %>% as.character()

constr_min <- constr_min %>%
  pivot_longer(.,
               cols = !!col_names,
               names_to = "year",
               values_to = "value"
               )


## add alphanumiso
conc_iso <- read_delim("./01_input/01_concordance_tables/source_country_ids.csv", ";")

constr_min <- constr_min %>% 
  left_join(.,
            conc_iso %>% 
              filter(source == "IRP_MFA") %>% 
              select(-source),
            by = c("Country" = "source_country_id")
  )


# check for missing countries in source_id table
nomatch_iso <- constr_min %>% 
  filter(is.na(alphanumiso)) %>% 
  group_by(Country) %>% 
  summarise(no_match = "no_match")

# a <- conc_iso %>% 
#   filter(source == "ISO_Alpha3") %>%
#   distinct(source_country_id, alphanumiso)

# a <- constr_min %>%
#   distinct(alphanumiso, Country) %>%
#   group_by(alphanumiso) %>%
#   summarise(n = n()) %>%
#   ungroup() %>%
#   filter(n > 1)
# 


# ## add material_ids
# conc_com <- read_delim("./01_input/01_concordance_tables/source_material_ids.csv", ";")
# 
# constr_min <- constr_min %>%
#   mutate(
#     material_id = case_when(
#       id == "gravel" ~ "M.sag",
#       id == "sand" ~ "M.sag",
#       id == "clay" ~ "M.coc",
#       id == "limestone" ~ "M.lim",
#       id == "gypsum" ~ "M.gya",
#     ))


constr_min <- constr_min %>%
  select(alphanumiso, material_id, year, value) %>%
  mutate(unit_id = "t") %>%
  mutate(year = as.integer(year)) %>%
  filter(!is.na(alphanumiso)) %>%
  filter(!is.na(material_id)) %>%
  filter(!is.na(value))


# convert to tonnes
constr_min <- constr_min %>%
  mutate(value = value * 1000)



# aggregate
constr_min <- constr_min %>%
  group_by(alphanumiso, material_id, year, unit_id) %>%
  summarise(value = sum(value, na.rm = TRUE))



# fill 2012:2019 with 2011
constr_min <- constr_min %>%
  union(.,
        constr_min %>%
          filter(year == 2011) %>%
          left_join(.,
                    tibble(year = 2011, new_year = 2012:2019),
                    by = NULL
          ) %>%
          mutate(year = new_year) %>%
          select(-new_year)
        )



### integrate with data

data <- read_rds("./03_intermediate/04_data_conversion/data_estimated_new.rds")

data <- data %>%
  full_join(.,
            constr_min,
            by = c("alphanumiso", "material_id", "year", "unit_id"),
            suffix = c("", "_est")
            ) %>%
  mutate(
    value = ifelse(
      is.na(value) & !is.na(value_est),
      value_est,
      value
    )) %>%
  mutate(
    value = ifelse(
      !is.na(value) & !is.na(value_est) & value_est > value,
      value_est,
      value
    ))

data <- data %>%
  select(-value_est)


# save rds
write_rds(data, "./03_intermediate/data_with_constr.rds")




a <- data %>%
      group_by(year) %>%
      summarise(value = sum(value, na.rm = TRUE))











