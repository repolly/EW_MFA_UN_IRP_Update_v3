##### Conversion of metal compounds to elemental metals
  ### Steps:
    # Remove lithium from conversion (being a very specific case)
    # Loop (for all relevant data sources)
      # Joining conversion factors
      # Converting values
      # Replacing material_ids






library(tidyverse)
library(rlang)

# clear R environment
rm(list = ls())



### load files

# harmonized data sets
wmd <- read_rds("./03_intermediate/02_data_harmonization/wmd_comb.rds")
unique(wmd$year)
bgs <- read_rds("./03_intermediate/02_data_harmonization/bgs_harmonized.rds")
unique(bgs$year)
usgs <- read_rds("./03_intermediate/02_data_harmonization/usgs_comb.rds")#usgs_comb_plus_ext.rds
usgs <- usgs[usgs$year>=2016,]
unique(usgs$year)
wmd_rep <- read_rds("./03_intermediate/02_data_harmonization/harm_wmd_rep.rds")
unique(wmd_rep$year)

# IDs and factors
mat_ids <- read_delim("./01_input/01_concordance_tables/material_ids.csv", ";")

conv_fac <- read_delim("./01_input/03_factors/conversion_elements.csv", ";")




## remove lithium from conversion (being a very specific case)
  ## requires no estimation in most cases, because either produced from salt brine or already reported as ore by BGS
  ## i.e. should be included as reported
  ## few cases which are estimated will use factors derived from WMD and BGS, i.e. estimating based on Me.Li2O and not on Me.Li
conv_fac <- conv_fac %>%
  filter(associated_material_id != "Me.Li")




# loop
  # joining factors
  # converting values
  # replacing material_ids
# i="wmd_ext"
for (i in c("wmd", "bgs", "usgs", "wmd_rep"))#, "wmd_ext")) 
  {

  df <- eval(parse(text = i)) %>%
  left_join(.,
            conv_fac %>%
              select(material_id, associated_material_id, factor)
            ) %>%
  mutate(
    value = ifelse(
      !is.na(associated_material_id),
      value * factor,
      value
      ),
    material_id = ifelse(
      !is.na(associated_material_id) & !is.na(factor),
      associated_material_id,
      material_id
    )
  ) %>%
    select(-associated_material_id, -factor)
  
  
  # aggregate
    # (because certain material might have been reported as metal and metal oxide and would now be present with two rows for each material per country and year)
  col_names <- names(df)[!names(df) %in% c("value")]

  df <- df %>%
    filter(!is.na(value)) %>%
    group_by(across(all_of(col_names))) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup()
  
  
  # replace initial datasets with adjusted tibble
  if(i == "wmd") wmd <- df
  if(i == "bgs") bgs <- df
  if(i == "usgs") usgs <- df
  if(i == "wmd_rep") wmd_rep <- df
  if(i == "wmd_ext") wmd_ext <- df

}



# save rds
write_rds(wmd, file = "./03_intermediate/04a_data_conversion/conversion_elements/wmd_conversion_1.rds")
write_rds(bgs, file = "./03_intermediate/04a_data_conversion/conversion_elements/bgs_conversion_1.rds")
write_rds(usgs, file = "./03_intermediate/04a_data_conversion/conversion_elements/usgs_conversion_1.rds")
write_rds(wmd_rep, file = "./03_intermediate/04a_data_conversion/conversion_elements/wmd_rep_conversion_1.rds")
# write_rds(wmd_ext, file = "./03_intermediate/04a_data_conversion/conversion_elements/wmd_ext_conversion_1.rds")



