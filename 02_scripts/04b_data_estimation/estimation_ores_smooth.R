##### Estimation of ores for relevant materials
  ### Steps:
    # Remove numbers smaller than or equal to 1 from GRU and SNL factors
    # Combine snl factors with gru factors
    # Remove outliers
    # Fill values for all years (i.e. 1970-2020)
    # Smooth
    # Create global averages from mining factors and integrate
    # Add old factors
    # Create global averages from old factors and integrate
    # Add additional factors
    # Create global averages from additional factors and integrate
    # Filter out Me.Li2O from global averages
    # Adjust specific cases
    # Apply estimations
    # Aggregate
    # Save all applied factors as csv
    # Save all integrated factors as csv









library(tidyverse)

# clear R environment
rm(list = ls())



### load files ----

## data
data <- read_rds("./03_intermediate/04a_data_conversion/conversion_units/data_converted_new.rds")


## factors

est_fac_gru <- read_delim("./01_input/03_factors/estimation_factors_new.csv", ";") %>%
  rename("material_id" = "commodity") %>%
  mutate(source = "based on FINEPRINT mining data and price-based allocation of ore")

est_fac_snl <- read_delim("./01_input/03_factors/estimation_factors_snl.csv", ";") %>%
  mutate(source = "based on S&P Global mining data and price-based allocation of ore")

# factors from old database
  # distinct entries, because contains double entries due to different unit factors in old database
est_fac_old <- read_delim("./01_input/03_factors/estimation_factors_old.csv", ";") %>%
  distinct(alphanumiso, material_id, Concentration) %>%
  mutate(source = "based on literature review")

# factors used in Excel files in the past
est_fac_old_2 <- read_delim("./01_input/03_factors/estimation_factors_old_2.csv", ";") %>%
  mutate(source = "based on factors applied in former compilation of database")

# additional factors
est_fac_add_1 <- read_delim("./01_input/03_factors/estimation_factors_add.csv", ";") %>%
  mutate(source = "global average based on data from WMD and BGS")

est_fac_add_2 <- read_delim("./01_input/03_factors/estimation_factors_add_2.csv", ";") %>%
  mutate(source = "based on data from WMD and BGS")

est_fac_add_3 <- read_delim("./01_input/03_factors/estimation_factors_add_3.csv", ";") %>%
  mutate(source = "based on data from WMD and BGS")

## IDs
mat_ids <- read_delim("./01_input/01_concordance_tables/material_ids.csv", ";")

est_ids <- read_delim("./01_input/01_concordance_tables/estimated_ids.csv", ";")








### remove numbers smaller than or equal to 1 from GRU and SNL factors ----
est_fac_snl <- est_fac_snl %>%
  filter(ratio_ore_per_met > 1)

est_fac_gru <- est_fac_gru %>%
  filter(ratio_ore_per_met > 1)






### check coverage of factors from GRU vs SNL
coverage <- est_fac_gru %>%
  group_by(alphanumiso, material_id) %>%
  summarise(gru = n()) %>%
  ungroup %>%
  full_join(.,
            est_fac_snl %>%
              group_by(alphanumiso, material_id) %>%
              summarise(snl = n()) %>%
              ungroup
            )


### combine snl factors with gru factors ----
  ## giving priority to snl factors, because nearly all snl time series have longer coverage (therefore being more consistent)
  ## + values from snl appear more consistent (likely due to higher underlying data coverage)

# filter gru
temp_1 <- setdiff(
  est_fac_gru %>%
    select(alphanumiso, material_id, year),
  est_fac_snl %>%
    select(alphanumiso, material_id, year)
  ) %>%
  left_join(est_fac_gru)


# combine
est_fac <- est_fac_snl %>%
  union(., temp_1)


# rename
est_fac <- est_fac %>%
  rename(factor = ratio_ore_per_met)


## remove outliers

# define ranges by IQR
ranges <- est_fac %>%
  group_by(material_id) %>%
  summarise(
    upper_range = quantile(factor, 3/4) + 3 * IQR(factor),
    lower_range = quantile(factor, 1/4) - 3 * IQR(factor)
    )

# remove 
est_fac <- est_fac %>%
  left_join(., ranges) %>%
  filter(
    factor < upper_range,
    factor > lower_range
  ) %>%
  select(-upper_range, -lower_range)

# a1 <- est_fac %>% 
#   filter(material_id == "Me.Cu") %>%
#   select(factor) %>%
#   pull()
#   
# IQR(a1)
# 
# quantile(a1, 2/4)





## fill values for all years (i.e. 1970-2020) ----

# create respective years
new_years <- tibble(
  year = 1970:2020,
  temp = "temp"
  ) %>%
  left_join(.,
            est_fac %>%
              distinct(alphanumiso, material_id) %>%
              mutate(temp = "temp")
            ) %>%
  select(-temp)

# join with table and arrange and fill (first up, then down)
extended <- new_years %>%
  left_join(., est_fac) %>%
  arrange(alphanumiso, material_id, year) %>%
  group_by(alphanumiso, material_id) %>%
  tidyr::fill(factor, .direction = "updown") %>%
  tidyr::fill(source, .direction = "updown") %>%
  ungroup()




# smooth ----
  # plus remove resulting negative values and refill with fill("updown")
factor_1 <- extended %>%
  group_by(alphanumiso, material_id) %>%
  mutate(ratio_sm = predict(loess(factor ~ year, span = 1, degree = 0))) %>%
  ungroup() %>%
  mutate(
    ratio_sm = ifelse(
      ratio_sm <= 0,
      NA,
      ratio_sm
    )
  ) %>%
  tidyr::fill(ratio_sm, .direction = "updown") %>%
  select(-factor) %>%
  rename(factor = ratio_sm)





## create global averages from mining factors and integrate ----

# for each material and year
global_1 <- factor_1 %>%
  group_by(year, material_id) %>%
  summarise(factor = mean(factor, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(source = "global average based on S&P Global / FINEPRINT mining data and price-based allocation of ore")


# get all rows of data where factor_1 has no factor
factor_2 <- setdiff(
  data %>%
    distinct(alphanumiso, material_id, year),
  factor_1  %>%
    distinct(alphanumiso, material_id, year)
  ) %>%
  left_join(., global_1) %>%
  filter(!is.na(factor))


# combine 
  # and include comment on source
all_factors <- factor_1 %>%
  union(., factor_2)


# # check for double counting
# all_factors %>%
#   group_by(alphanumiso, material_id, year) %>%
#   summarise(n = n()) %>%
#   ungroup() %>%
#   filter(n > 1)






## add old factors ----

# turn concentration into estimation factor (i.e. t/t) in order to be consistent with other factors
est_fac_old <- est_fac_old %>%
  mutate(factor = 1/Concentration) %>%
  select(-Concentration)

# combine with est_fac_old_2
est_fac_old <- est_fac_old %>%
  union(.,
        est_fac_old_2 %>%
          rename(factor = est_fac)
        )


# get all rows of data where all_factors has no factor
factor_3 <- setdiff(
  data %>%
    distinct(alphanumiso, material_id, year),
  all_factors  %>%
    distinct(alphanumiso, material_id, year)
) %>%
  left_join(., est_fac_old) %>%
  filter(!is.na(factor))


# combine 
all_factors <- all_factors %>%
  union(., factor_3)




## create global averages from old factors and integrate ----

# for each material
global_2 <- est_fac_old %>%
  group_by(material_id) %>%
  summarise(factor = mean(factor, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(source = "global average based on factors from literature review")


# get all rows of data where all_factors has no factor
factor_4 <- setdiff(
  data %>%
    distinct(alphanumiso, material_id, year),
  all_factors  %>%
    distinct(alphanumiso, material_id, year)
) %>%
  left_join(., global_2) %>%
  filter(!is.na(factor))


# combine 
all_factors <- all_factors %>%
  union(., factor_4)






## add additional factors ----

# combine est_add_2 and est_add_3
  # both country-specific and non-annual and based on WMD-BGS
est_fac_add <- est_fac_add_2 %>%
  union(., est_fac_add_3)


# get all rows of data where all_factors has no factor
factor_5 <- setdiff(
  data %>%
    distinct(alphanumiso, material_id, year),
  all_factors  %>%
    distinct(alphanumiso, material_id, year)
) %>%
  left_join(., est_fac_add) %>%
  filter(!is.na(factor))


# combine 
all_factors <- all_factors %>%
  union(., factor_5)



## create global averages from additional factors and integrate ----

# for each material
global_3 <- est_fac_add %>%
  group_by(material_id) %>%
  summarise(factor = mean(factor, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(source = "global average based on data from WMD and BGS")


# add est_fac_add_1
  # because they're also non-annual and global average
global_3 <- global_3 %>%
  union(.,
        est_fac_add_1 %>%
          rename(factor = est_fac)
  )


# filter out Me.Li2O from global averages
  # because should only be estimated for countries where factor already exists in table
  # as everything else is produced from salt brines
global_3 <- global_3 %>%
  filter(material_id != "Me.Li2O")


# get all rows of data where all_factors has no factor
factor_6 <- setdiff(
  data %>%
    distinct(alphanumiso, material_id, year),
  all_factors  %>%
    distinct(alphanumiso, material_id, year)
) %>%
  left_join(., global_3) %>%
  filter(!is.na(factor))


# combine 
all_factors <- all_factors %>%
  union(., factor_6)














## adjust specific cases ----

# remove Me.Li
  # should not be estimated (because likely from salt brine if reported as Me.Li)
  # estimated only for specific cases kept as Me.LiO2
all_factors <- all_factors %>%
  filter(material_id != "Me.Li")

# temporarily remove Me.Al for estimations
  # therefore only including reported Bauxite, but not estimated
  # but keep in file all_factors_temp in order to save at the end
all_factors_temp <- all_factors

all_factors <- all_factors %>%
  filter(material_id != "Me.Al")


  






## apply estimations ----
new_data <- data %>%
  left_join(.,
            all_factors %>%
              select(-source)
            ) %>%
  mutate(
    value = ifelse(
      !is.na(factor),
      value * factor,
      value
    ))
        


# check for missing IDs for estimated materials
no_match_est_id <- new_data %>%
  filter(!is.na(factor)) %>%
  left_join(.,
            est_ids %>%
              filter(category == "associated") %>%
              select(ore_id, material_id)
  ) %>%
  filter(is.na(ore_id)) %>%
  distinct(material_id)


# save a table with the actually applied factors
applied_factors <- new_data %>%
  filter(!is.na(factor)) %>%
  select(year, alphanumiso, material_id) %>%
  left_join(., all_factors)


# change respective material_id where an estimation factor was available (and keep material_id if none was available)
new_data <- new_data %>%
  left_join(.,
            est_ids %>%
              filter(category == "associated") %>%
              select(ore_id, material_id)
            ) %>%
  mutate(material_id = ifelse(
    is.na(factor),
    material_id,
    ore_id
    )
  ) %>% 
  mutate(source = ifelse(
    is.na(factor),
    source,
    paste0("own estimation based on ", source)
    )
  )





# remove obsolete columns
new_data <- new_data %>%
  select(-factor, -ore_id)






# aggregate
new_data <- new_data %>%
  filter(!is.na(value)) %>%
  group_by(alphanumiso, material_id, year, unit_id, source) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()




# check if specific entries (i.e. alphanumiso-material_id-year) have more than one source after estimation
a1 <- new_data %>%
  group_by(alphanumiso, material_id, year, unit_id) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n > 1)





## save rds ----
write_rds(new_data, "./03_intermediate/4b_data_estimation/data_estimated_ores.rds")




## arrange and save final est_fac ----

# save all applied factors as csv
applied_factors <- applied_factors %>%
  select(alphanumiso, material_id, year, factor, source) %>%
  arrange(material_id, alphanumiso, year)

write_delim(
  applied_factors,
  paste0(
    "./04_output/05_supplementary_info/applied_est_fac_",
    substr(Sys.time(),1,10),
    ".csv"
  ), 
  delim = ";", 
  na = "")



# save all integrated factors as csv
all_factors_temp <- all_factors_temp %>%
  select(alphanumiso, material_id, year, factor, source) %>%
  arrange(material_id, alphanumiso, year)

write_delim(
  all_factors_temp,
  paste0(
    "./04_output/05_supplementary_info/all_integrated_est_fac_",
    substr(Sys.time(),1,10),
    ".csv"
  ), 
  delim = ";", 
  na = "")







