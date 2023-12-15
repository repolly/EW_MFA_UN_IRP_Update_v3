##### Integration of all fossil fuels (UNSD, IEA, EIA)
  ### Steps:
    # Save table with all data combined and aggregate everything for countries which merged across time
      # Aggregate all in loop based on cou_for_aft
    # Integration UNSD with IEA
      # Compare coverage of UNSD and IEA for each material per country and decide which source to take
        # Comparison coal
          # UNSD filtered for coal
          # UNSD coal -> number of years with value > 0 per country
          # IEA filtered for coal
          # IEA coal -> number of years with value > 0 per country
          # Make a decision (i.e. iea only if coverage at least 50% higher)
          # Filter UNSD data for respective entries
          # Filter IEA data for respective entries
        # IEA coal prior 1991
      # Comparison non-coal fossil fuels (incl. peat)
        # Similar to coal above
      # Integrate all parts
      # Remove obsolete time series from all datasets (i.e. only values = 0)
    # Integration unsd_iea with EIA
      # Similar to UNSD-IEA above
    # Add disaggregated data again
    







library(tidyverse)

# clear R environment
rm(list = ls())



## read files

old_output <- read_rds("./04_output/02_final/DE_met_min_fos_Detailed_2021-11-04.rds")
old_eia <- old_output[old_output$source=="eia",] # need to continue this timelines for eia -> 546 rows
old_iea <- old_output[old_output$source=="iea",] # need to continue this timelines for iea -> 5645 rows

# data
unsd <- read_rds("./03_intermediate/02_data_harmonization/unsd.rds")

iea <- read_rds("./03_intermediate/02_data_harmonization/iea.rds")

eia <- read_rds("./03_intermediate/02_data_harmonization/eia.rds")

# drop timelines that need to continue in for the other source
# IEA to drop
rows_iea_to_drop <- which(((iea$alphanumiso %in% old_eia$alphanumiso)+(iea$material_id %in% old_eia$material_id))==2)
iea_to_drop <- iea[rows_iea_to_drop,]
iea <- iea[-rows_iea_to_drop,]

# EIA to drop
rows_eia_to_drop <- which(((eia$alphanumiso %in% old_iea$alphanumiso)+(eia$material_id %in% old_iea$material_id))==2)
eia_to_drop <- eia[rows_eia_to_drop,]
eia <- eia[-rows_eia_to_drop,]

# IDs
mat_ids <- read_delim("./01_input/01_concordance_tables/material_ids.csv", delim = ";")

# lists
cou_for_aft <- read_delim("./01_input/01_concordance_tables/country_former_after.csv", delim = ";")




## save table with all data combined and aggregate everything for countries which merged across time
  ## do integration with aggregated table in order to avoid double counting and re-combine with disaggregated data at end
prior_data <- unsd %>% mutate(source = "unsd") %>%
  bind_rows(., iea %>% mutate(source = "iea")) %>%
  bind_rows(., eia %>% mutate(source = "eia"))


# aggregate all in loop based on cou_for_aft
for (i in c("unsd", "iea", "eia")) {
  
  col_names <- names(eval(parse(text = i)))[names(eval(parse(text = i))) != "value"]
  
  df <- eval(parse(text = i)) %>%
    left_join(.,
              cou_for_aft %>%
                filter(category == "merge") %>%
                select(-category) %>%
                rename(alphanumiso = former)
    ) %>%
    mutate(
      alphanumiso = ifelse(
        !is.na(after),
        after,
        alphanumiso
      )
    ) %>%
    group_by(across(all_of(col_names))) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup()
  
  if(i == "unsd") unsd <- df
  if(i == "iea") iea <- df
  if(i == "eia") eia <- df

}





# maximum year across all three datasets (in order to only compare coverage of same time range)
max_y <- min(
  unsd %>% select(year) %>% max(),
  iea %>% select(year) %>% max(),
  eia %>% select(year) %>% max()
  )




#### Integration UNSD with IEA -------


### Compare coverage of UNSD and IEA for each material per country and decide which source to take
  ### For coal (excl. peat) from 1991 onwards, because before 1991 coal is only taken from IEA due to disaggregation of coal categories
    ### 1991 and not 1990, because DEU somehow has no values from UNSD in 1990
  ### For all other materials from 1970 onwards
  ### IEA only taken when coverage at least 50% higher 


## comparison coal ------
  ## coal is always only taken from one source per country (due to potential different reporting/allocation per coal category)
    ## however, comparison not by sum of coal, but by number for all coal types, because otherwise potential error source
    ## (i.e. for cases where data is missing for some coal types except for one coal type in one source, 
    ## it would still have aggregated values for all years where data is missing)


# unsd filtered for coal
coal_unsd <- unsd %>%
  left_join(.,
            mat_ids %>%
              select(material_id, material_category_2)
            ) %>%
  filter(
    material_category_2 == "coal & peat",
    material_id != "F.pea"
  ) %>%
  select(-material_category_2)

# unsd coal -> number of years with value > 0 per country
coal_unsd_cov <- coal_unsd %>%
  filter(
    value > 0,
    year <= max_y & year >= 1991) %>%
  group_by(alphanumiso) %>%
  summarise(n = n()) %>%
  ungroup()


# iea filtered for coal
coal_iea <- iea %>%
  left_join(.,
            mat_ids %>%
              select(material_id, material_category_2)
  ) %>%
  filter(
    material_category_2 == "coal & peat",
    material_id != "F.pea"
  ) %>%
  select(-material_category_2)

# iea coal -> number of years with value > 0 per country
coal_iea_cov <- coal_iea %>%
  filter(
    value > 0,
    year >= 1991 & year <= max_y) %>%
  group_by(alphanumiso) %>%
  summarise(n = n()) %>%
  ungroup()


# both
comp_coal_cov <- coal_unsd_cov %>%
  full_join(.,
            coal_iea_cov,
            by = c("alphanumiso"),
            suffix = c("_unsd", "_iea")
            )


# make a decision (i.e. iea only if coverage at least 50% higher)
comp_coal_cov <- comp_coal_cov %>%
  mutate(
    dec = case_when(
      is.na(n_unsd) ~ "iea",
      is.na(n_iea) ~ "unsd",
      n_iea >= (1.5 * n_unsd) ~ "iea",
      n_iea < (1.5 * n_unsd) ~ "unsd",
    )) %>%
  select(alphanumiso, dec)


# adjustment of special case: UKR804
  # UNSD not reporting proper data before 2007, wherefore IEA data needs to be used
comp_coal_cov <- comp_coal_cov %>%
  mutate(dec = ifelse(
    alphanumiso == "UKR804",
    "iea",
    dec
  ))


# number of countries per source
comp_coal_cov %>%
  group_by(dec) %>%
  summarise(n = n())


# filter UNSD data for respective entries
unsd_part_1 <- coal_unsd %>%
  filter(year >= 1991) %>%
  filter(
    alphanumiso %in% (comp_coal_cov %>% 
                        filter(dec == "unsd") %>%
                        select(alphanumiso) %>%
                        pull()
                      )
    )


# filter IEA data for respective entries
iea_part_1 <- coal_iea %>%
  filter(year >= 1991) %>%
  filter(
    alphanumiso %in% (comp_coal_cov %>% 
                        filter(dec == "iea") %>%
                        select(alphanumiso) %>%
                        pull()
                      )
    )



## IEA coal prior 1991
iea_coal_hist <- coal_iea %>%
  filter(year < 1991) 






## comparison non-coal fossil fuels (incl. peat) ------

# unsd filtered for other
other_unsd <- unsd %>%
  left_join(.,
            mat_ids %>%
              select(material_id, material_category_2)
  ) %>%
  filter(material_category_2 != "coal & peat" | material_id == "F.pea") %>%
  select(-material_category_2)

# unsd other -> number of years with value > 0 per country
other_unsd_cov <- other_unsd %>%
  group_by(alphanumiso, material_id, year) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(
    value > 0,
    year <= max_y) %>%
  group_by(alphanumiso, material_id) %>%
  summarise(n = n()) %>%
  ungroup()


# iea filtered for other
other_iea <- iea %>%
  left_join(.,
            mat_ids %>%
              select(material_id, material_category_2)
  ) %>%
  filter(material_category_2 != "coal & peat" | material_id == "F.pea") %>%
  select(-material_category_2)

# iea other -> number of years with value > 0 per country
other_iea_cov <- other_iea %>%
  group_by(alphanumiso, material_id, year) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(
    value > 0,
    year <= max_y) %>%
  group_by(alphanumiso, material_id) %>%
  summarise(n = n()) %>%
  ungroup()


# both
comp_other_cov <- other_unsd_cov %>%
  full_join(.,
            other_iea_cov,
            by = c("alphanumiso", "material_id"),
            suffix = c("_unsd", "_iea")
  )


# make a decision (i.e. iea only if coverage at least 50% higher)
comp_other_cov <- comp_other_cov %>%
  mutate(
    dec = case_when(
      is.na(n_unsd) ~ "iea",
      is.na(n_iea) ~ "unsd",
      n_iea >= (1.5 * n_unsd) ~ "iea",
      n_iea < (1.5 * n_unsd) ~ "unsd",
    )) %>%
  select(alphanumiso, material_id, dec)


# number of countries per source
comp_other_cov %>%
  group_by(dec) %>%
  summarise(n = n())


# filter UNSD data for respective entries
unsd_part_2 <- comp_other_cov %>% 
  filter(dec == "unsd") %>%
  select(alphanumiso, material_id) %>%
  left_join(., 
            other_unsd,
            by = c("alphanumiso", "material_id")
            )

# filter IEA data for respective entries
iea_part_2 <- comp_other_cov %>% 
  filter(dec == "iea") %>%
  select(alphanumiso, material_id) %>%
  left_join(., 
            other_iea,
            by = c("alphanumiso", "material_id")
  )





## integrate all parts
unsd_iea <- unsd_part_1 %>%
  mutate(source = "unsd") %>%
  union(., iea_part_1 %>%
          mutate(source = "iea")
        ) %>%
  union(., iea_coal_hist %>%
          mutate(source = "iea")
        ) %>%
  union(., unsd_part_2 %>%
          mutate(source = "unsd")
        ) %>%
  union(., iea_part_2 %>%
          mutate(source = "iea")
        )





## Remove obsolete time series from all datasets (i.e. only values = 0)
unsd_iea <- unsd_iea %>% 
  group_by(alphanumiso, material_id) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(value != 0) %>%
  select(-value) %>%
  left_join(., unsd_iea)




# number of countries per source
unsd_iea %>%
  group_by(source) %>%
  summarise(n = n())









#### Integration unsd_iea with EIA -------


### Compare coverage of unsd_iea and EIA for each material per country and decide which source to take
### Here for all materials from 1970 onwards
  ### Even though EIA only reports from 1980 onwards (except for crude oil), because data prior 1980 is relevant and therefore needs to be taken into account
  ### But still in two steps, for coal (excl. peat) and for other materials (EIA data not incl. other hydrocarbons and oil sand)
### EIA only taken when coverage at least 50% higher than unsd_iea


## comparison coal ------
## coal is always only taken from one source per country (due to potential different reporting/allocation per coal category)
## therefore comparison for sum of coal


# unsd_iea filtered for coal
coal_unsd_iea <- unsd_iea %>%
  left_join(.,
            mat_ids %>%
              select(material_id, material_category_2)
  ) %>%
  filter(
    material_category_2 == "coal & peat",
    material_id != "F.pea"
  ) %>%
  select(-material_category_2)

# unsd_iea coal -> number of years with value > 0 per country
coal_unsd_iea_cov <- coal_unsd_iea %>%
  group_by(alphanumiso, year) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(
    value > 0,
    year <= max_y) %>%
  group_by(alphanumiso) %>%
  summarise(n = n()) %>%
  ungroup()


# EIA filtered for coal
coal_eia <- eia %>%
  left_join(.,
            mat_ids %>%
              select(material_id, material_category_2)
  ) %>%
  filter(
    material_category_2 == "coal & peat",
    material_id != "F.pea"
  ) %>%
  select(-material_category_2)

# EIA coal -> number of years with value > 0 per country
coal_eia_cov <- coal_eia %>%
  group_by(alphanumiso, year) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(
    value > 0,
    year <= max_y) %>%
  group_by(alphanumiso) %>%
  summarise(n = n()) %>%
  ungroup()


# both
comp_coal_cov <- coal_unsd_iea_cov %>%
  full_join(.,
            coal_eia_cov,
            by = c("alphanumiso"),
            suffix = c("_unsd_iea", "_eia")
  )


# make a decision (i.e. EIA only if coverage at least 50% higher)
comp_coal_cov <- comp_coal_cov %>%
  mutate(
    dec = case_when(
      is.na(n_unsd_iea) ~ "eia",
      is.na(n_eia) ~ "unsd_iea",
      n_eia >= (1.5 * n_unsd_iea) ~ "eia",
      n_eia < (1.5 * n_unsd_iea) ~ "unsd_iea",
    )) %>%
  select(alphanumiso, dec)


# number of countries per source
comp_coal_cov %>%
  group_by(dec) %>%
  summarise(n = n())


# filter unsd_iea for respective entries
unsd_iea_part_1 <- coal_unsd_iea %>%
  filter(
    alphanumiso %in% (comp_coal_cov %>% 
                        filter(dec == "unsd_iea") %>%
                        select(alphanumiso) %>%
                        pull()
    )
  )


# filter EIA data for respective entries
eia_part_1 <- coal_eia %>%
  filter(
    alphanumiso %in% (comp_coal_cov %>% 
                        filter(dec == "eia") %>%
                        select(alphanumiso) %>%
                        pull()
    )
  )









## comparison non-coal fossil fuels (incl. peat) ------

# unsd_iea filtered for other
other_unsd_iea <- unsd_iea %>%
  left_join(.,
            mat_ids %>%
              select(material_id, material_category_2)
  ) %>%
  filter(material_category_2 != "coal & peat" | material_id == "F.pea") %>%
  select(-material_category_2)

# unsd_iea other -> number of years with value > 0 per country
other_unsd_iea_cov <- other_unsd_iea %>%
  group_by(alphanumiso, material_id, year) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(
    value > 0,
    year <= max_y) %>%
  group_by(alphanumiso, material_id) %>%
  summarise(n = n()) %>%
  ungroup()


# EIA filtered for other
other_eia <- eia %>%
  left_join(.,
            mat_ids %>%
              select(material_id, material_category_2)
  ) %>%
  filter(material_category_2 != "coal & peat" | material_id == "F.pea") %>%
  select(-material_category_2)

# EIA other -> number of years with value > 0 per country
other_eia_cov <- other_eia %>%
  group_by(alphanumiso, material_id, year) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(
    value > 0,
    year <= max_y) %>%
  group_by(alphanumiso, material_id) %>%
  summarise(n = n()) %>%
  ungroup()


# both
comp_other_cov <- other_unsd_iea_cov %>%
  full_join(.,
            other_eia_cov,
            by = c("alphanumiso", "material_id"),
            suffix = c("_unsd_iea", "_eia")
  )


# make a decision (i.e. EIA only if coverage at least 50% higher)
comp_other_cov <- comp_other_cov %>%
  mutate(
    dec = case_when(
      is.na(n_unsd_iea) ~ "eia",
      is.na(n_eia) ~ "unsd_iea",
      n_eia >= (1.5 * n_unsd_iea) ~ "eia",
      n_eia < (1.5 * n_unsd_iea) ~ "unsd_iea",
    )) %>%
  select(alphanumiso, material_id, dec)


# number of countries per source
comp_other_cov %>%
  group_by(dec) %>%
  summarise(n = n())


# filter unsd_iea for respective entries
unsd_iea_part_2 <- comp_other_cov %>% 
  filter(dec == "unsd_iea") %>%
  select(alphanumiso, material_id) %>%
  left_join(., 
            other_unsd_iea,
            by = c("alphanumiso", "material_id")
  )

# filter EIA data for respective entries
eia_part_2 <- comp_other_cov %>% 
  filter(dec == "eia") %>%
  select(alphanumiso, material_id) %>%
  left_join(., 
            other_eia,
            by = c("alphanumiso", "material_id")
  )





## integrate all parts
fossils <- unsd_iea_part_1 %>%
  union(., eia_part_1 %>%
          mutate(source = "eia")
        ) %>%
  union(., unsd_iea_part_2) %>%
  union(., eia_part_2 %>%
          mutate(source = "eia"))





## Remove obsolete time series from all datasets (i.e. only values = 0)
fossils <- fossils %>% 
  group_by(alphanumiso, material_id) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(value != 0) %>%
  select(-value) %>%
  left_join(., fossils)



fossils %>% filter(is.na(value))



## add disaggregated data again
  # taking into account to only include the geographic entity selected by integration and being available in initial data
fossils <- fossils %>%
  filter(
    alphanumiso %in% (cou_for_aft %>%
                        filter(category == "merge") %>%
                        distinct(after) %>%
                        pull()
    )
  ) %>%
  left_join(.,
            cou_for_aft %>%
              filter(category == "merge") %>%
              select(-category) %>%
              rename(alphanumiso = after)
  ) %>%
  select(former, year, material_id, source) %>%
  rename(alphanumiso = former) %>%
  union(.,
        fossils %>%
          select(alphanumiso, year, material_id, source)
  ) %>%
  left_join(., prior_data)


# remove resulting NAs from re-adding disaggregated data
fossils <- fossils %>%
  filter(!is.na(value))






# number of data points per source
fossils %>%
  group_by(source) %>%
  summarise(n = n())

# f1=fossils[fossils$year>=2011,]

# save rds
write_rds(fossils, file = "./03_intermediate/03_data_integration/fossils_integrated.rds")



