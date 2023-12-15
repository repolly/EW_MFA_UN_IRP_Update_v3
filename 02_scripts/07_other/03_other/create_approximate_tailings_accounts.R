
######## Approximation of tailings


### Using harmonized, not integrated, not converted
  ### Converting units
  ### Converting to metal compounds (where possible/necessary)
  ### Integrate (i.e. filter by source and use setdiff for associated_mat_id)
  ### Calculating tailings vs final detailed data
    ### Matching by associated IDs
  ### Create two tables, one with detailed, one with CCC (both including names)


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



# harmonized data sets (before everything else)
wmd <- read_rds("./03_intermediate/02_data_harmonization/wmd_comb.rds")
bgs <- read_rds("./03_intermediate/02_data_harmonization/bgs_harmonized.rds")
usgs <- read_rds("./03_intermediate/02_data_harmonization/usgs_comb_plus_ext.rds")



# IDs and factors
mat_ids <- read_delim("./01_input/01_concordance_tables/material_ids.csv", ";")

ccc_vs_mat <- read_delim("./01_input/01_concordance_tables/ccc_vs_mat_ids.csv", delim = ";")

mfa13_ids <- read_delim("./01_input/01_concordance_tables/CCC_To_EWMFA_4_and_13_20171010.csv", delim = ";")

cou_ids <- read_delim("./01_input/01_concordance_tables/country_ids.csv", delim = ";")

est_ids <- read_delim("./01_input/01_concordance_tables/estimated_ids.csv", ";")

conv_fac <- read_delim("./01_input/03_factors/conversion_factors.csv", ";")

conv_elem <- read_delim("./01_input/03_factors/conversion_elements.csv", ";")








### harmonized data ----

# put harmonized tables together
# only for wmd, bgs, usgs
data <- wmd %>%
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
  arrange(alphanumiso, year, material_id)



# include mat_categories and associated IDs
data <- data %>%
  left_join(
    mat_ids %>%
      select(material_id, material_category, associated_mat_id)
    )


# filter for commodities
data <- data %>%
  filter(material_category == "commodity")






### unit conversion ----

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


# remove entries which are not in "t"
data <- data %>%
  filter(unit_id == "t")


## aggregate
data <- data %>%
  group_by(alphanumiso, material_id, year, unit_id, associated_mat_id, source) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()






### conversion to metal compounds (for elemental metals) ----

# convert
  # removing two entries for Lithium (to not create false double counting)

data <- data %>%
  left_join(.,
            conv_elem %>%
              select(material_id, associated_material_id, factor) %>%
              filter(!material_id %in% c("Me.Li2CO3", "Me.LiOH")) %>%
              rename(
                material_id_conv = material_id,
                material_id = associated_material_id
                )
            ) %>%
  mutate(
    value = ifelse(
      !is.na(material_id_conv),
      value / factor,
      value
    ),
    material_id = ifelse(
      !is.na(material_id_conv) & !is.na(factor),
      material_id_conv,
      material_id
    )
  ) %>%
  select(-material_id_conv, -factor)




# aggregate
  # (because certain material might have been reported as metal and metal oxide and would now be present with two rows for each material per country and year)
col_names <- names(data)[!names(data) %in% c("value")]

data <- data %>%
  filter(!is.na(value)) %>%
  group_by(across(all_of(col_names))) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()




### integrate ----

# set NA for associated_mat_id = material_id
data <- data %>%
  mutate(associated_mat_id = ifelse(
    is.na(associated_mat_id),
    material_id,
    associated_mat_id
    ))


## create integrated data table and add filter data step by step

# all from wmd
data_integr <- data %>% 
  filter(source == "wmd")


# all from bgs not in wmd
temp <- setdiff(
  data %>% 
    filter(source == "bgs") %>%
    select(alphanumiso, year, associated_mat_id),
  data %>% 
    filter(source == "wmd") %>%
    select(alphanumiso, year, associated_mat_id)
  ) %>%
  left_join(.,
            data %>%
              filter(source == "bgs")
            )

# wmd-bgs
data_integr <- data_integr %>% 
  union(., temp)


# all from usgs not in wmd-bgs
temp <- setdiff(
  data %>% 
    filter(source == "usgs") %>%
    select(alphanumiso, year, associated_mat_id),
  data_integr %>%
    select(alphanumiso, year, associated_mat_id)
) %>%
  left_join(.,
            data %>%
              filter(source == "usgs")
  )


# wmd-bgs-usgs
data_integr <- data_integr %>% 
  union(., temp)






### calculate tailings ----

# filter out commodities and include associated IDs
  # filtering not based on material_category, but "O." and "AO.", because easier this way
  # inclusion of associated IDs based on mat_ids and on est_ids
all_data <- final_detailed %>%
  filter(grepl("O.|AO.", material_id)) %>%
  left_join(
    mat_ids %>%
      select(material_id, associated_mat_id)
    ) %>%
  left_join(
    est_ids %>%
      select(ore_id, material_id) %>%
      rename(
        associated_mat_id_2 = material_id,
        material_id = ore_id
      )
  ) %>%
  mutate(
    associated_mat_id = ifelse(
      is.na(associated_mat_id),
      associated_mat_id_2,
      associated_mat_id
    )
  ) %>%
  select(-associated_mat_id_2)


# combine both tables (i.e. final data and integrated commodity data)
all_data <- all_data %>%
  select(alphanumiso, CCC_Code, material_id, associated_mat_id, year, value) %>%
  left_join(.,
            data_integr %>%
              select(alphanumiso, material_id, associated_mat_id, year, value),
            by = c("alphanumiso", "year", "associated_mat_id"),
            suffix = c("_ore", "_com")
            )


# aggregate by associated id, because of different ores per metal (e.g. Me.Ti)
all_data <- all_data %>%
  group_by(alphanumiso, CCC_Code, associated_mat_id, year) %>%
  summarise(
    value_ore = sum(value_ore, na.rm = TRUE),
    value_com = sum(value_com, na.rm = TRUE)
  )


# calculate tailings
all_data <- all_data %>%
  mutate(tailings = value_ore - value_com)


# remove negative values for tailings
all_data <- all_data %>%
  filter(tailings > 0)


# join with final_detailed again to include names
all_data <- all_data %>%
  left_join(.,
            final_detailed %>%
              distinct(alphanumiso, CCC_Code, country, CCC_Name, MFA13)
            ) %>%
  left_join(.,
            mat_ids %>%
              select(material_id, material_name),
            by = c("associated_mat_id" = "material_id")
            )


# aggregate to CCC
col_names <- names(all_data)[!names(all_data) %in% c("associated_mat_id", "material_name","value_ore", "value_com", "tailings")]

aggregated_ccc <- all_data %>%
  group_by(across(all_of(col_names))) %>%
  summarise(
    value_ore = sum(value_ore, na.rm = TRUE),
    value_com = sum(value_com, na.rm = TRUE),
    tailings = sum(tailings, na.rm = TRUE)
    ) %>%
  ungroup()


# save data as csv
write_delim(
  all_data,
  paste0(
    "./04_output/06_other/tailings/approximation_tailings_detailed_",
    substr(Sys.time(),1,10),
    ".csv"
  ), 
  delim = ";", 
  na = "")

# save data as csv
write_delim(
  aggregated_ccc,
  paste0(
    "./04_output/06_other/tailings/approximation_tailings_ccc_",
    substr(Sys.time(),1,10),
    ".csv"
  ), 
  delim = ";", 
  na = "")







