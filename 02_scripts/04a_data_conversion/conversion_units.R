##### Convert all units to metric tons
  ### Steps:
    # Check missing conversion factors
      # in general (especially for mass units)
      # for material-specific cases (i.e. volume/energy)
    # Add factor and convert 
      # for mass
      # for volume/energy
      # for volume/energy in case that factor for material_id is missing (temporary, until entries are added)
    # Remove entries which are not in "t"
    # Aggregate







library(tidyverse)

# clear R environment
rm(list = ls())



# load files
data <- read_rds("./03_intermediate/03_data_integration/all_data_integrated.rds")

conv_fac <- read_delim("./01_input/03_factors/conversion_factors.csv", ";")








## check missing conversion factors

# in general (especially for mass units)
setdiff(
  data %>% 
    select(unit_id),
  conv_fac %>%
    select(unit_id)
  )

# for material-specific cases (i.e. volume/energy)
  # certain materials will always appear as no proper conversion factors could be found (i.e. needs to be checked intuitively)
setdiff(
  data %>% 
    left_join(.,
              conv_fac %>% 
                filter(unit_cat %in% c("volume", "energy")) %>%
                distinct(unit_id, unit_cat)
    ) %>%
    filter(!is.na(unit_cat)) %>%
    distinct(material_id, unit_id),
  conv_fac %>%
    filter(unit_cat %in% c("volume", "energy")) %>%
    distinct(material_id, unit_id)
  )




## add factor and convert 

# unit category
data <- data %>% 
  left_join(.,
            conv_fac %>% 
              distinct(unit_id, unit_cat)
  )


# for mass
data <- data %>% 
  left_join(.,
            conv_fac %>% 
              filter(unit_cat == "mass") %>%
              select(unit_id, conv_factor_t)
            ) %>% 
  mutate(
    value = ifelse(
      is.na(conv_factor_t),
      value, 
      value * conv_factor_t
      ),
    unit_id = ifelse(
      is.na(conv_factor_t), 
      unit_id, 
      "t" )
    ) %>% 
  select(-conv_factor_t)


# for volume/energy
data <- data %>% 
  left_join(.,
            conv_fac %>% 
              filter(unit_cat %in% c("volume", "energy")) %>%
              select(material_id, unit_id, conv_factor_t)
  ) %>% 
  mutate(
    value = ifelse(
      is.na(conv_factor_t),
      value, 
      value * conv_factor_t
    ),
    unit_id = ifelse(
      is.na(conv_factor_t), 
      unit_id, 
      "t" )
  ) %>% 
  select(-conv_factor_t)



# for volume/energy in case that factor for material_id is missing (temporary, until entries are added)
  # i.e. applying general/average values for these cases
data <- data %>% 
  left_join(.,
            conv_fac %>% 
              filter(unit_cat %in% c("volume", "energy")) %>%
              filter(is.na(material_id)) %>%
              select(unit_id, conv_factor_t)
  ) %>% 
  mutate(
    value = ifelse(
      is.na(conv_factor_t),
      value, 
      value * conv_factor_t
    ),
    unit_id = ifelse(
      is.na(conv_factor_t), 
      unit_id, 
      "t" )
  ) %>% 
  select(-conv_factor_t)



# check for entries not being "t"
a1 <- data %>%
  filter(unit_id != "t")


# remove entries which are not in "t"
  # in order to avoid wrong data for units with no proper conversion factors
  # removing some entries temporarily for those cases, where missing entries are added or errors are corrected
  # removing some entries in general for those, where simply no suitable conversion factor has been found 
data <- data %>%
  filter(unit_id == "t")



## aggregate
data <- data %>%
  group_by(alphanumiso, material_id, year, unit_id, source) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()

# data <- data[data$year>=2016,]

# save rds
write_rds(data, "./03_intermediate/04a_data_conversion/conversion_units/data_converted_new.rds")




