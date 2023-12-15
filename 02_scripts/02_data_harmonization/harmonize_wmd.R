##### Harmonize all datasets from WMD
  ### Steps:
    # Harmonize recently downloaded data (i.e. for five latest years)
      # Add alphanumiso
        # Check for missing allocations
      # Add material_ids
        # Check for missing allocations
      # Add unit_ids
        # Check for missing allocations
      # Aggregate
    # Harmonize information on data quality (for years 2015-2017)
      # Add alphanumiso
        # Check for missing allocations
      # Add material_ids
        # Check for missing allocations






library(tidyverse)

# clear R environment
rm(list = ls())




### read files ----

## data

# recent data
wmd <- read_rds("./03_intermediate/02_data_harmonization/clean_wmd_rec.rds")
# unique(wmd$year)
# historical data
# wmd_hist <- read_rds("./03_intermediate/02_data_harmonization/clean_wmd_hist.rds")

# info on data quality
wmd_rep <- read_rds("./03_intermediate/02_data_harmonization/clean_wmd_reported_15_17.rds")


## concordance tables
source_cou_ids <- read_delim("./01_input/01_concordance_tables/source_country_ids.csv", delim = ";")

source_mat_ids <- read_delim("./01_input/01_concordance_tables/source_material_ids.csv", delim = ";")

source_unit_ids <- read_delim("./01_input/01_concordance_tables/source_unit_ids.csv", delim = ";")


## special cases
spec_case <- read_delim("./01_input/02_log_files/wmd/special_cases.csv", delim = ";")








### harmonize wmd recent data ----

# convert class types to be consistent
wmd$year <- as.integer(wmd$year)
wmd$value <- as.numeric(wmd$value)



## add alphanumiso
wmd <- wmd %>% 
  left_join(.,
    source_cou_ids %>% 
       filter(source == "WMD") %>% 
       select(-source),
    by = c("country" = "source_country_id")
    )


# check for missing alphanumiso
no_match_cou <- wmd %>% 
  filter(is.na(alphanumiso)) %>% 
  group_by(country) %>% 
  summarise(no_match = "no_match")

# save no_match table
write_delim(no_match_cou, file = "./04_output/01_harmonization/01_conc_nomatch/wmd_no_match_country.csv")




## add material_ids
wmd <- wmd %>% 
  left_join(
    source_mat_ids %>%
      filter(source == "WMD") %>%
      select(-source),
    by = c("commodity" = "source_material_id")
  )

# check for missing material_id
no_match_mat <- wmd %>% 
  filter(is.na(material_id)) %>% 
  group_by(commodity) %>% 
  summarise(no_match = "no_match")

# save no_match table
write.csv(no_match_mat, file = "./04_output/01_harmonization/01_conc_nomatch/wmd_no_match_mat.csv")




## add unit_ids

# convert Mio m3, in case it gets read in different than the excel
wmd <- wmd %>% 
  mutate(
    unit = ifelse(unit == "Mio m³", "Mio m3", unit)
    )


wmd <- wmd %>% 
  left_join(
    source_unit_ids %>% 
      filter(source == "WMD") %>% 
      select(-source),
    by = c("unit" = "source_unit_id")
    )

# check for missing unit_ids
no_match_unit <- wmd %>% 
  filter(is.na(unit_id)) %>% 
  group_by(unit) %>% 
  summarise(no_match = "no_match")

# save no_match table as csv file
write.csv(no_match_unit, file = paste0("./04_output/01_harmonization/01_conc_nomatch/wmd_no_match_unit.csv"))




# aggregate
wmd <- wmd %>%
  filter(!is.na(value)) %>%
  group_by(alphanumiso, material_id, year, source, unit_id) %>% 
  summarize(value = sum(value, na.rm = TRUE)) %>% 
  ungroup()





## format

# remove NAs in material_id and alphanumiso
wmd <- wmd %>% 
  filter(!is.na(material_id),
         !is.na(alphanumiso))



# add values for reported and estimated to column "rep" 
wmd <- wmd %>% 
  mutate(source = ifelse(
    source %in% c("r", "reported"), 
    "r", 
    source
    ))

wmd <- wmd %>%
  rename("rep" = "source")



## adjust special cases
wmd <- wmd %>%
  left_join(.,
            spec_case %>%
              select(-comment)
            ) %>%
  mutate(
    alphanumiso = ifelse(
      !is.na(alphanumiso_new) & year >= year_start & year <= year_end, 
      alphanumiso_new, 
      alphanumiso
    )) %>%
  select(-alphanumiso_new, -year_start, -year_end)



# sort data
wmd <- wmd %>% 
  arrange(alphanumiso, material_id, year)



# remove problematic mat_ids (because likely not representing primary extraction)
  # maybe temporary, because they could still be included independent of extraction data (for other data applications)
wmd <- wmd %>%
  filter(!(material_id %in% c("Me.Fe")))



# save rds
write_rds(wmd, file = "./03_intermediate/02_data_harmonization/wmd_comb.rds")











#### harmonize info on data quality (i.e. info on sources) ----

wmd_rep$year <- as.integer(wmd_rep$year)


## add alphanumiso
wmd_rep <- wmd_rep %>% 
  left_join(.,
    source_cou_ids %>% 
      filter(source == "WMD") %>% 
      select(-source), 
    by = c("country" = "source_country_id")
    )

# check for missing alphanumiso
no_match_cou <- wmd_rep %>% 
  filter(is.na(alphanumiso)) %>% 
  group_by(country) %>% 
  summarise(no_match = "no_match")

write.csv(no_match_cou, file = "./04_output/01_harmonization/01_conc_nomatch/wmd_rep_no_match_cou.csv")



## add material_ids
source_mat_ids <- readr::read_delim("./01_input/01_concordance_tables/source_material_ids.csv", ";")

wmd_rep <- wmd_rep %>% 
  left_join(.,
    source_mat_ids %>% 
      filter(source == "WMD") %>% 
      select(-source), 
    by = c("commodity" = "source_material_id"))

# check for missing material_ids
no_match_mat <- wmd_rep %>% 
  filter(is.na(material_id)) %>% 
  group_by(commodity) %>% 
  summarise(no_match = "no_match")

write.csv(no_match_mat, file = "./04_output/01_harmonization/01_conc_nomatch/wmd_rep_no_match_mat.csv")



## remove obsolete columns
wmd_rep <- wmd_rep %>%  select(-c("country","commodity"))



## keep only cases with material_id, alphanumiso and source info (i.e. "rep") 
  # (about 10 cases where there is NA in rep column)

wmd_rep <- wmd_rep %>%
  filter(!is.na(material_id),
         !is.na(alphanumiso),
         !is.na(rep)
         )


# save rds
write_rds(wmd_rep, file = "./03_intermediate/02_data_harmonization/harm_wmd_rep.rds")




