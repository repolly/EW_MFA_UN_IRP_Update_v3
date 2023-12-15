# save an intermediate file to store all kinds of materials
  # i.e. before estimation and after including construction minerals (for comparison with mining data)



library(tidyverse)

# clear R environment
rm(list = ls())



# load files
data_before <- read_rds("./03_intermediate/04_data_conversion/data_converted_new.rds")

data_after <- read_rds("./03_intermediate/data_with_constr.rds")



# combine
all_materials <- data_before %>%
  union(., data_after)




# remove obsolete time series (i.e. only values = 0)
all_materials <- all_materials %>% 
  group_by(alphanumiso, material_id) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(value != 0) %>%
  select(-value) %>%
  left_join(., all_materials)




# save file
write_delim(
  all_materials,
  paste0(
    "./04_output/03_comparison/national_accounts_all_materials_",
    substr(Sys.time(),1,10),
    ".csv"
  ), 
  delim = ";", 
  na = "")




