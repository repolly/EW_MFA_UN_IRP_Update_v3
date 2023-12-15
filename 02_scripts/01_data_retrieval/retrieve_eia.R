##### Retrieval of EIA data
  ### Via API of EIA (please note: key needed -> see .Renviron file)
  ### API documentation: https://www.eia.gov/opendata/commands.php
  ### PLEASE NOTE: EIA has released a new API version in 2022
  ### Steps:
    # Make sure EIA commodity/category IDs are still valid
    # Two nested loops for download
      # Outer loop to retrieve series identifiers for category/material and filter for required series_ids
        # Remove series_ids which cause double counting
      # Inner loop to retrieve data for each series_id
        # Combine data at end of loop




library(jsonlite)
library(httr)
library(tidyverse)

# clear R environment
rm(list = ls())


# set environment variables
# i.e. API key
# readRenviron("./02_scripts/02_country_specific/eia/.Renviron")

eia_api_key <- "Rb6GcAhpskDIokJrsYjs406whT7MEMlQqbgsaGXC"



# IDs for the materials which are retrieved from EIA
cat_ids <- c("2134611", # Dry Natural Gas
             "2134979", # Production of Crude Oil including Lease Condensate
             "2134995", # NGPL
             "2134515", # Anthracite
             "2134531", # Bituminous
             "2134563", # Lignite
             "2135283", # Subbituminous             
             "2135299") # Metallurgical coal



# remove EIA tibble if in R environment
if(exists("eia")) rm(eia)



#i <- 2135299

for (i in cat_ids[4:8]) {
  
  # retrieve series identifiers for category from API
  # includes all available datasets for this material (different versions, different units, different reporting periods)
  url <- paste0("https://api.eia.gov/category/?api_key=", eia_api_key, "&category_id=", i)
  
  series_id <- fromJSON(url)
  
  series_id <- series_id$category$childseries
  
  # specifying units
  # many materials are reported in different units
    # however, according to a check on number of units per material, all entries should be available for each country using these units
    # furthermore, in the current EIA data structure, these units also avoid double counting, with only one possible unit per material
  units <- c("thousand barrels per day", "billion cubic feet", "thousand short tons") 
  
  # filter for required entries
  # i.e. proper units and proper reporting period
  series_id <- series_id %>%
    filter(units %in% !!units) %>%
    filter(f == "A") %>% # Annual
    select(series_id)
  
  # remove identifiers including "WP", as they represent double entries for a selection of countries
  # (e.g. Brazil: INTL.26-1-BRA-BCF vs INTL.26-1-WP13-BCF.A)
  series_id <- series_id %>%
    filter(!grepl("-WP", series_id))
  
  
  
  #j <- "INTL.57-1-ABW-TBPD.A"
  
  # retrieve data for the respective cat_id
  # temi = which(i==cat_ids)
  # tem = which(j==series_id$series_id)
  # len = length(series_id$series_id)
  for (j in series_id$series_id){#[tem:len]) {
    
    print(j)
    
    json_list <- fromJSON(paste0("https://api.eia.gov/series/?api_key=", eia_api_key, "&series_id=", j))
    
    # columns with general info (country code, unit, material, time range)
    general <- json_list[["series"]][c("name","units","geography","series_id","start","end")] %>%
      as_tibble() %>%
      mutate(temp = NA)
    
    # columns with years and values
    values <- json_list[["series"]][["data"]] %>% 
      as.data.frame(., stringsAsFactors = FALSE) %>%
      as_tibble() %>%
      mutate(temp = NA)
    
    # combine both tibbles
    data <- general %>%
      full_join(., values)
    
    
    # union each retrieved tibble from loop into one
    if(exists("eia")) {eia <- eia %>% union(., data)} else {eia <- data}
    
    
  }
  
}
#[1] "INTL.57-1-TCD-TBPD.A"



# remove obsolete column
eia <- eia %>%
  select(-temp)



# save rds
write_rds(eia, file = paste0("/03_intermediate/01_data_retrieval/eia/eia_dldate_", substr(Sys.time(),1,10),".rds"))



