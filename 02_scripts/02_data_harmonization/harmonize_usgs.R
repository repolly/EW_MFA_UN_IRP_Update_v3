##### Harmonize downloaded and former data from USGS (not USGS manual extensions)
  ### Steps:
    # Harmonize downloaded data
      # Add alphanumiso
        # Check for missing allocations
      # Add unit_ids
        # Check for missing allocations
      # Add material_ids
        # Check for missing allocations
    # Remove values which have been identified as wrong







library(tidyverse)

# clear R environment
rm(list = ls())



## read files

# recent data
usgs <- read_rds("./03_intermediate/02_data_harmonization/clean_usgs.rds") %>%
  ungroup()
usgs <- usgs[usgs$year>=2016,]
# historic data 
# usgs_hist <- read.table("./01_input/04_former_data/usgs/t_data_data_20170328.txt",  sep="\t", header=TRUE, na.strings = "XX")








### harmonize recent USGS data --------------------------

usgs$year <- as.integer(usgs$year)

## Alphanumiso
conc_iso <- readr::read_delim("./01_input/01_concordance_tables/source_country_ids.csv", ";")

usgs <- usgs %>% 
  left_join(
    conc_iso %>% 
      filter(source == "USGS") %>% 
      select(-source),
    by = c("country" = "source_country_id")
  )

# check for missing matches of the join and give out a report (names in sources change from time to time)

nomatch_iso <- usgs %>% 
  filter(is.na(alphanumiso)) %>% 
  group_by(country) %>% 
  summarise(no_match = "no_match")

write.csv(nomatch_iso, file = "./04_output/01_harmonization/01_conc_nomatch/usgs_nomatch_iso.csv")

### Unit IDs
conc_units <- readr::read_delim("./01_input/01_concordance_tables/source_unit_ids.csv", ";")

usgs <- usgs %>% 
  left_join(
    conc_units %>%
      filter(source == "USGS") %>%
      select(-source),
    by = c("unit" = "source_unit_id")
  )

#check for missing unit_ids
nomatch_unit <- usgs %>% 
  filter(is.na(unit_id)) %>% 
  group_by(unit) %>% 
  summarise(no_match = "no_match")

#save as csv file
write_delim(nomatch_unit, file = paste0("./04_output/01_harmonization/01_conc_nomatch/usgs_nomatch_unit.csv"), delim = ";")

### commodity IDS

conc_com <- readr::read_delim("./01_input/01_concordance_tables/source_material_ids.csv", ";")

usgs <- usgs %>% 
  left_join(
    conc_com %>% 
      filter(source == "USGS") %>% 
      select(-source), 
    by = c("commodity" = "source_material_id")
  )

# check for missings
nomatch_com <- usgs %>% 
  filter(is.na(material_id)) %>% 
  group_by(commodity) %>% 
  summarise(no_match = "no_match")

write_delim(nomatch_com, file = "./04_output/01_harmonization/01_conc_nomatch/usgs_nomatch_com.csv", delim = ";")


#delete missings of alphanumiso and material_id
usgs <- usgs %>% filter(!is.na(material_id)) 
usgs <- usgs %>% filter(!is.na(alphanumiso))

usgs <- usgs %>%
  filter(!is.na(value)) %>%
  group_by(alphanumiso, material_id, unit_id, year) %>% 
  summarize(value = sum(value, na.rm = TRUE)) %>%
  ungroup()





# save rds
write_rds(usgs, file = "./03_intermediate/02_data_harmonization/usgs_comb.rds")







