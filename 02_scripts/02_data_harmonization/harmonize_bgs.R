##### Harmonize data from BGS
    ### Steps:
      # Remove problematic mat_ids (because likely not representing primary extraction)
      # Add alphanumiso
        # Check for missing allocations
      # Adjust geographic inconsistencies (based on geo_exist.csv and formula geographic_adjust.R)
      # Add material_ids
        # Check for missing allocations
      # Add material_ids for BGS sub-commodity column
        # Check for missing allocations
      # Adjustment of special cases for material_ids (e.g. missing sub-commodity entries or difference in reported materials)
      # Add unit_ids
        # Check for missing allocations
      # Adjustment of special cases which are wrong in BGS data
      # Harmonize footnote column
      # Aggregate
      # Harmonize data on sulphur from former database
        # Combine with downloaded data 








library(tidyverse)
library(readxl)

# clear R environment
rm(list = ls())



### read files ----

## data (most recent file)

source("./02_scripts/00_functions/lastfile.R")

lastfile <- my_lastfile("./03_intermediate/02_data_harmonization/", "clean_bgs")

bgs <- read_rds(lastfile)

#drop years before 2015 ... due to the comparison in the integration
bgs <- bgs[bgs$year>=2015,]

# former database
  # only for sulphur
# gru_sql_2017 <- read.table("./01_input/04_former_data/usgs/t_data_data_20170328.txt",  sep="\t", header=TRUE, na.strings = "XX")


## concordance tables
source_cou_ids <- read_delim("./01_input/01_concordance_tables/source_country_ids.csv", delim = ";")

source_mat_ids <- read_delim("./01_input/01_concordance_tables/source_material_ids.csv", delim = ";")

source_unit_ids <- read_delim("./01_input/01_concordance_tables/source_unit_ids.csv", delim = ";")


# load special cases
spec_case <- read_delim("./01_input/02_log_files/bgs/special_cases.csv", ";")









### harmonization ----


# remove problematic mat_ids (because likely not representing primary extraction)
  # maybe temporary, because they could still be included independent of extraction data (for other data applications)
bgs <- bgs %>%
  filter(!(commodity %in% c("Production of alumina", "Production of cobalt metal")))



# vector of min and max year
years <- min(bgs$year):max(bgs$year)



## add alphanumiso
bgs <- bgs %>% 
  left_join(
    source_cou_ids %>% 
      filter(source == "BGS") %>% 
      select(-source),
    by = c("country" = "source_country_id")
    )

# check for missing alphanumiso
nomatch_iso <- bgs %>% 
  filter(is.na(alphanumiso)) %>% 
  group_by(country) %>% 
  summarise(no_match = "no_match")

# save no_match table
write.csv(nomatch_iso, file = "./04_output/01_harmonization/01_conc_nomatch/bgs_nomatch_iso.csv")




## adjust geographic inconsistencies (e.g. Soviet Union vs successor states)

# add info on "former countries"
geo_exist  <- readr::read_delim("./01_input/01_concordance_tables/geo_exist.csv", ";")

bgs <- bgs %>% 
  left_join(.,
            geo_exist %>% select("alphanumiso", "former"),
            by = c("alphanumiso")
            )

# apply geographic adjust formula in order to restructure countries
source("./02_scripts/00_functions/geographic_adjust.R")

bgs <- geographic_adjust(bgs)




## add material_ids 
source_mat_ids <- readr::read_delim("./01_input/01_concordance_tables/source_material_ids.csv", ";")

bgs <- bgs %>% 
  left_join(
    source_mat_ids %>%
      filter(source == "BGS") %>% 
      select(-source), 
    by = c("commodity" = "source_material_id")
    )

# check for missing material_ids
nomatch_com <- bgs %>% 
  filter(is.na(material_id)) %>% 
  group_by(commodity) %>% 
  summarise(no_match = "no_match")

write.csv(nomatch_com, file = "./04_output/01_harmonization/01_conc_nomatch/bgs_nomatch_com.csv")



# add material_ids for BGS sub-commodity column
bgs <- bgs %>% 
  left_join(
    source_mat_ids %>% 
      filter(source == "BGS") %>% 
      select(-source), 
    by = c("sub_commodity" = "source_material_id"),
    suffix = c("", "_sub")
    )

# check for missing material_ids for sub-commodities (most of these are irrelevant, but check still relevant)
  # not worth it including an "exclude"-table here, as it's not done often enough and variables don't change that often
nomatch_com_sub <- bgs %>% 
  filter(is.na(material_id_sub)) %>% 
  group_by(commodity, sub_commodity, country) %>% 
  summarise(no_match = "no_match")



# combine both material_id columns (i.e. for commodity and sub-commodity)
bgs <- bgs %>% 
  mutate(material_id_new =  coalesce(material_id, material_id_sub)) %>%
  select(-c(material_id, material_id_sub)) %>%
  rename("material_id" = "material_id_new")



## adjustment of special cases for material_ids (e.g. missing sub-commodity entries or difference in reported materials)
  ## please note: by coincidence only those cases for specific countries also involve some with specific time ranges
  ## -> therefore if-conditions are held simple based on that

year_max <- max(bgs$year)


# via loop
  # because same material more than once for some countries (i.e. for different years), which otherwise causes false rows to be included
  # and which otherwise would require to create table with row for each specific year

ph <- bgs
bgs <- ph

i <- 2

for (i in 1:nrow(spec_case)) {
  
  # two if-conditions
  # first, general cases without country or specific time range
  
  if(is.na(spec_case[i,]$country)) {
  
  bgs <- bgs %>%
    left_join(.,
              spec_case[i,] %>%
                select(commodity, sub_commodity, material_id),
              by = c("commodity", "sub_commodity"),
              suffix = c("", "_spec")
    ) %>%
    mutate(
      material_id = ifelse(!is.na(material_id_spec), material_id_spec, material_id)
    ) %>%
    select(-material_id_spec)
  }
  
  
  # second, cases which are specific for country and time range
  if(!is.na(spec_case[i,]$country)) {
  
  bgs <- bgs %>%
    left_join(.,
              spec_case[i,] %>%
                select(-comment) %>%
                mutate(
                  year_end = ifelse(
                    is.na(year_end) & !is.na(year_start),
                    year_max, # i.e. automatically using the latest year of dataset
                    year_end)
                ),
              by = c("country",	"commodity", "sub_commodity"),
              suffix = c("", "_spec")
    ) %>%
    mutate(
      material_id = ifelse(
        !is.na(material_id_spec) & year >= year_start & year <= year_end, 
        material_id_spec, 
        material_id
      )) %>%
    mutate(
      material_id = ifelse(
        !is.na(material_id_spec) & is.na(year_start), 
        material_id_spec, 
        material_id
      )) %>%
    select(-material_id_spec, -year_start, -year_end)
  }
  
  }




### add unit_ids
source_unit_ids <- readr::read_delim("./01_input/01_concordance_tables/source_unit_ids.csv", ";")

bgs <- bgs %>% 
  left_join(
    source_unit_ids %>% 
      filter(source == "BGS") %>% 
      select(-source), 
    by = c("unit" = "source_unit_id")
  )

# check for missing unit_ids
nomatch_unit <- bgs %>% 
  filter(is.na(unit_id)) %>% 
  group_by(unit) %>% 
  summarise(no_match = "no_match")

# save no_match table as csv file
write.csv(nomatch_unit, file = paste0("./04_output/01_harmonization/01_conc_nomatch/bgs_nomatch_unit.csv"))


# remove obsolete columns
bgs <- bgs %>% 
  select(-c(country, commodity, sub_commodity, unit, former))



# remove NAs
bgs <- bgs %>%
  filter(!is.na(material_id)) %>%
  filter(!is.na(value)) %>%
  filter(!is.na(alphanumiso)) %>%
  filter(!is.na(unit_id))









### harmonize footnote column -----

ph <- bgs
bgs <- ph


# set all "*" to "e" (which are estimates) and everything else to "r" (i.e. reported)
bgs$rep[grep("\\*", bgs$rep)] <- "e"
bgs$rep[bgs$rep != "e" | is.na(bgs$rep)] <- "r"


# just set all footnotes to "e" where at least one "e" exists within the same combination of alphanumiso-material_id-year
  # (i.e. within those which are aggregated)

bgs <- bgs %>%
  group_by(alphanumiso, material_id, year) %>%
  mutate(all_rep = paste(rep, collapse = " | ")) %>%
  ungroup() %>%
  mutate(rep = ifelse(grepl("e", all_rep), "e", rep))







# aggregate
bgs <- bgs %>%
  filter(!is.na(value)) %>%
  group_by(alphanumiso, material_id, unit_id, year, rep) %>% 
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()


# check whether there are still "duplicates" for combinations of alphanumiso - material_id - year
bgs %>% 
  group_by(alphanumiso, material_id, year, rep) %>%  
  summarise(n = n()) %>% 
  filter(n > 1)









# ### harmonize data on sulphur from former database ----
# 
# ## format
# bgs_hist <- gru_sql_2017 %>% 
#   filter(SourceID == 227 & CommodityID %in% c(89, 90)) %>% 
#   select(CountryID, YearID, CommodityID, PrimaryValue, UnitID, SourceID, UsedValue)
# 
# bgs_hist <- bgs_hist %>% 
#   rename(
#     "country" = "CountryID",
#     "year" = "YearID",
#     "commodity" = "CommodityID",
#     "unit" = "UnitID",
#     "value" = "PrimaryValue"
#   ) %>% 
#   select(-SourceID)
# 
# 
# 
# 
# ## Alphanumiso
# source_cou_ids <- readr::read_delim("./01_input/01_concordance_tables/country_ids.csv", ";")
# bgs_hist <- bgs_hist %>% 
#   left_join((source_cou_ids  %>% select(alphanumiso, wu_former_id)), by = c("country" = "wu_former_id"))
# 
# # check for missing matches of the join and give out a report (names in sources change from time to time)
# 
# nomatch_iso <- bgs_hist %>% 
#   filter(is.na(alphanumiso)) %>% 
#   group_by(country) %>% 
#   summarise(no_match = "no_match")
# 
# write.csv(nomatch_iso, file = "./04_output/01_harmonization/01_conc_nomatch/bgs_hist_nomatch_iso.csv")
# 
# ### Unit IDs
# 
# source_unit_ids <- readr::read_delim("./01_input/01_concordance_tables/unit_ids.csv", ";")
# bgs_hist <- bgs_hist %>% 
#   left_join((source_unit_ids %>% select(unit_id, wu_former)), by = c("unit" = "wu_former")) 
# 
# #check for missing unit_ids
# nomatch_unit <- bgs_hist %>% 
#   filter(is.na(unit_id)) %>% 
#   group_by(unit) %>% 
#   summarise(no_match = "no_match")
# 
# #save as csv file
# write.csv(nomatch_unit, file = paste0("./04_output/01_harmonization/01_conc_nomatch/bgs_hist_nomatch_unit.csv"))
# 
# # just as a check if needed whether there are NAs: bgs_hist %>% filter(is.na(gru_unit_id)) %>% select(unit) %>% group_by(unit) %>% summarize(n = n())
# 
# ### material_ids
# source_mat_ids <- readr::read_delim("./01_input/01_concordance_tables/source_material_ids.csv", ";")
# 
# #change datatype to do the merge
# bgs_hist$commodity <- as.character(bgs_hist$commodity) 
# 
# # add data
# bgs_hist <- bgs_hist %>% 
#   left_join(
#     source_mat_ids %>% 
#       filter(source == "Former_db") %>% 
#       select(-source),
#     by = c("commodity" = "source_material_id")
#   )
# 
# # check for missings
# nomatch_com <- bgs_hist %>% 
#   filter(is.na(material_id)) %>% 
#   group_by(commodity) %>% 
#   summarise(no_match = "no_match")
# 
# write.csv(nomatch_com, file = "./04_output/01_harmonization/01_conc_nomatch/bgs_hist_nomatch_com.csv")
# 
# 
# 
# #delete missings of alphanumiso and material_id
# bgs_hist <- bgs_hist %>% filter(!is.na(material_id)) 
# bgs_hist <- bgs_hist %>% filter(!is.na(alphanumiso))
# 
# bgs_hist <- bgs_hist %>% 
#   filter(!is.na(value)) %>%
#   group_by(alphanumiso, material_id, unit_id, year) %>% 
#   summarize(value = sum(value, na.rm = TRUE)) %>%
#   ungroup()





# ## combine data
# bgs <- bgs %>%
#   bind_rows(., bgs_hist)





# save rds
write_rds(bgs, file = "./03_intermediate/02_data_harmonization/bgs_harmonized.rds")


