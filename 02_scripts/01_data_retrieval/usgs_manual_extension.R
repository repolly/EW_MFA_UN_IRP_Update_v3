#### Create table for manual USGS extension


library(tidyverse)
library(openxlsx)


# clear R environment
rm(list = ls())



# load files
data <- read_rds("./03_intermediate/03_data_integration/all_data_integrated.rds")
# data[((data$year==2019)+(data$source == "usgs"))==2,]
# data[((data$year==2019)+(data$source == "usgs"))==2,]
  
# data <- read_rds("./03_intermediate/03_data_integration/all_data_integrated_old.rds")
# data %>%
#   filter(source == "usgs") %>% filter(source == "2019")
  
source_mat_ids <- read_delim("./01_input/01_concordance_tables/source_material_ids.csv", delim = ";")

mat_ids <- read_delim("./01_input/01_concordance_tables/material_ids.csv", delim = ";")

cou_ids <- read_delim("./01_input/01_concordance_tables/country_ids.csv", delim = ";")

unit_ids <- read_delim("./01_input/01_concordance_tables/unit_ids.csv", delim = ";")

conv_fac <- read_delim("./01_input/03_factors/conversion_factors.csv", delim = ";")






# filter for all material time series which are fully or partly from USGS
usgs_series <- data %>%
  filter(source == "usgs") %>%
  distinct(alphanumiso, material_id) %>%
  left_join(., data)

unique(usgs_series$year)
# usgs[usgs$year==2019,]

# save tibble as csv
  # in order to be able to include all extended data later while removing data points from other sources
  # and though keeping sources correct
write_delim(
  usgs_series,
  "./03_intermediate/01_data_retrieval/usgs/extension/all_sources_usgs_time_series.csv",
  delim = ";",
  na = ""
  )

unique(usgs_series$year)

# consolidate source column
usgs_series <- usgs_series %>%
  distinct(alphanumiso, material_id, source) %>%
  group_by(alphanumiso, material_id) %>%
  summarise(source_all = paste(source, collapse = ", ")) %>%
  ungroup() %>%
  left_join(.,
            usgs_series %>%
              select(-source)
            )

unique(usgs_series$year)

# include material and country names
usgs_series <- usgs_series %>%
  left_join(.,
            mat_ids %>%
              select(material_id, material_name)
            ) %>%
  left_join(.,
            cou_ids %>%
              select(alphanumiso, country)
  ) %>%
  select(alphanumiso, country, material_id, material_name, year, unit_id, source_all, value)

unique(usgs_series$year)

# turn into wide format and arrange accordingly
usgs_series <- usgs_series %>%
  arrange(year, alphanumiso, material_name) %>%
  pivot_wider(.,
              names_from = year,
              values_from = value
              ) %>%
  arrange(alphanumiso, material_name)
  


# add column to mark new entries
usgs_series <- usgs_series %>%
  add_column(., "new_entry" = NA, .after = "2018") %>%
  add_column(., "unit" = NA, .after = "unit_id") %>%
  mutate(across(where(is.logical), as.character))







## create table with overview of source_material namings from USGS for each material_id
source_materials <- usgs_series %>%
  distinct(material_id) %>%
  left_join(.,
            source_mat_ids %>%
              filter(source %in% c("USGS", "USGS_ext")) %>%
              select(-source)
            ) %>%
  arrange(material_id, source_material_id)








## create the same table and info for the other data
  ## (in order to be able to check if specific materials are already covered by WMD and BGS)

# filter for all material time series which are not fully or partly from USGS
  # and excluding info from unsd, iea, eia
non_usgs_series <- setdiff(
  data %>%
    filter(!source %in% c("unsd", "iea", "eia")) %>%
    distinct(alphanumiso, material_id),
  usgs_series %>%
    distinct(alphanumiso, material_id)
  ) %>%
  left_join(., data)


# consolidate source column
non_usgs_series <- non_usgs_series %>%
  distinct(alphanumiso, material_id, source) %>%
  group_by(alphanumiso, material_id) %>%
  summarise(source_all = paste(source, collapse = ", ")) %>%
  ungroup() %>%
  left_join(.,
            non_usgs_series %>%
              select(-source)
  )



# include material and country names
non_usgs_series <- non_usgs_series %>%
  left_join(.,
            mat_ids %>%
              select(material_id, material_name)
  ) %>%
  left_join(.,
            cou_ids %>%
              select(alphanumiso, country)
  ) %>%
  select(alphanumiso, country, material_id, material_name, year, unit_id, source_all, value)



# turn into wide format and arrange accordingly
non_usgs_series <- non_usgs_series %>%
  arrange(year, alphanumiso, material_name) %>%
  pivot_wider(.,
              names_from = year,
              values_from = value
  ) %>%
  arrange(alphanumiso, material_name)








## create table with overview of source_material namings from USGS for each material_id
source_materials_wmd_bgs <- non_usgs_series %>%
  distinct(material_id) %>%
  left_join(.,
            source_mat_ids %>%
              filter(source %in% c("WMD", "BGS", "WMD_ext")) %>%
              select(-source)
  ) %>%
  arrange(material_id, source_material_id)










# save all tables into Excel file
write.xlsx(
  x = list(
    data = usgs_series, 
    source_materials = source_materials, 
    unit_ids = unit_ids,
    conversion_factors = conv_fac,
    other_data = non_usgs_series#,
    #other_source_materials = source_materials_wmd_bgs
    ),
  file = paste0(
    "./03_intermediate/01_data_retrieval/usgs/extension/extension_", 
    substr(Sys.time(),1,10), 
    ".xlsx"
    ),
  overwrite = TRUE
  )

write_delim(
  non_usgs_series,
  paste0(
    "./03_intermediate/01_data_retrieval/usgs/extension/extension_sheet_other_data_", 
    substr(Sys.time(),1,10), 
    ".csv"
  ),
  delim = ";",
  na = ""
)

write_delim(
  source_materials_wmd_bgs,
  paste0(
    "./03_intermediate/01_data_retrieval/usgs/extension/extension_sheet_source_mat_wmd_bgs_", 
    substr(Sys.time(),1,10), 
    ".csv"
  ),
  delim = ";",
  na = ""
)
  






