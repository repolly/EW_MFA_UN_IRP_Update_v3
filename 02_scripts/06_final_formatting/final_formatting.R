##### Final formatting
  ### Steps:
    # Remove obsolete time series (i.e. only values = 0)
    # Check for missing mat_id-CCC allocations
    # Fill all years for all A.3 after 2011 until max year of each CCC for each country
    # Aggregate
    # Save CCC data as csv
    # Save CCC data with names as csv
    # Save disaggregated data as rds
    # Save disaggregated data as csv
    # Save disaggregated data with names as csv
    # Yearly sums (for checking)







library(tidyverse)
library(scales)

# clear R environment
rm(list = ls())



## read files

# data
data <- read_rds("./03_intermediate/06_final_formatting/adjusted_outliers.rds")#tested with final_cleaning.rds

#Adjustment 1: AGO & F.cro -> Unit error
data[((data$alphanumiso=="AGO024")+(data$material_id=="F.cro")+(data$year>=2016))==3,6] <- data[((data$alphanumiso=="AGO024")+(data$material_id=="F.cro")+(data$year>=2016))==3,6]/10


# IDs
ccc_vs_mat <- read_delim("./01_input/01_concordance_tables/ccc_vs_mat_ids.csv", delim = ";")

cou_ids <- read_delim("./01_input/01_concordance_tables/country_ids.csv", delim = ";", locale = locale(encoding = "UTF-8"))

mfa13_ids <- read_delim("./01_input/01_concordance_tables/CCC_To_EWMFA_4_and_13_20171010.csv", delim = ";")

mat_ids <- read_delim("./01_input/01_concordance_tables/material_ids.csv", delim = ";")

# lists
cou_for_aft <- read_delim("./01_input/01_concordance_tables/country_former_after.csv", delim = ";")







## clean

# remove obsolete time series (i.e. only values = 0)
data <- data %>% 
  group_by(alphanumiso, material_id) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(value != 0) %>%
  select(-value) %>%
  left_join(., data)




# add concordance 
data <- data %>%
  left_join(.,
            ccc_vs_mat %>%
              distinct(material_id, CCC_Code)
  )


# check for missing mat_id-CCC allocations
data %>%
  filter(is.na(CCC_Code)) %>%
  distinct(material_id)




# filter
data <- data %>% 
  filter(!is.na(CCC_Code)) %>%
  rename(unit = unit_id)





## get overview of distribution of most recent years after 2015 (before gap filling)
latest_year_distr <- data %>%
  group_by(alphanumiso, material_id) %>%
  summarise(latest_year = max(year, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(latest_year) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(latest_year)) %>%
  filter(latest_year >= 2015) %>%
  mutate(share = percent(n/sum(n)))








## fill all years for all A.3 after 2011 until max year of each CCC for each country ----
  ## (using tidyr::fill in a grouped table)
  ## 2011 being the latest year included from former construction mineral estimations, 2018 being the latest USGS year
  ## in order to avoid drops in time series of CCC due to different data coverage of underlying materials
    ## i.e. fill all gaps between years in that time range
    ## i.e. for recent years where underlying materials for each CCC have different ending years for their time series
      ## i.e. if a material has a recent value for specific year (e.g. 2013), but is NA afterwards, while other materials in same ccc have a value,
      ## then fill up NAs for recent years with this most recent value, in order to have a consistent ccc
  ## please note: this might lead to overestimating production for some cases: where a primary source reported NA instead of 0 
    ## can't currently be avoided, as initial NAs weren't kept (i.e. would require an additional formatting of all initial NAs before processing) 


# maximum year for each CCC
year_max <- data %>%
  filter(grepl("A.3", CCC_Code)) %>%
  group_by(CCC_Code, alphanumiso) %>%
  summarise(y_max = max(year, na.rm = TRUE)) %>%
  ungroup()

# add mat_ids for each CCC
year_max <- year_max %>%
  left_join(.,
            data %>%
              distinct(CCC_Code, alphanumiso, material_id)
            )

# filter for maximum years > 2011
year_max <- year_max %>%
  filter(y_max > 2011)

# get tibble with all required years for all alphanumiso-material_id
all_years <- tibble(
  CCC_Code = NA,
  alphanumiso = NA,
  material_id = NA,
  year = NA) %>%
  mutate(across(c(CCC_Code, alphanumiso, material_id), as.character)) %>%
  mutate(across(year, as.integer)) %>%
  filter(!is.na(year))

for (i in 1:nrow(year_max)) {
  
  a <- tibble(
    CCC_Code = year_max$CCC_Code[i],
    alphanumiso = year_max$alphanumiso[i],
    material_id = year_max$material_id[i],
    year = c(2011:year_max$y_max[i])
  )
  
  all_years <- all_years %>%
    union(., a)
}


# extend all time series where necessary
temp <- all_years %>%
  left_join(.,
            data %>%
              filter(grepl("A.3", CCC_Code))
            ) %>%
  arrange(CCC_Code, alphanumiso, material_id, year) %>%
  group_by(CCC_Code, alphanumiso, material_id) %>%
  tidyr::fill(value, .direction = "downup") %>%
  ungroup() %>%
  mutate(unit = "t") %>%
  mutate(source = ifelse(
    is.na(source),
    "own estimation - gap filled by values of preceding or consecutive years",
    source
  ))


# re-combine with data
new_data <- data %>%
  union(., temp) %>%
  filter(!is.na(value))

# format disaggregated data
new_data <- new_data %>%
  select(alphanumiso, CCC_Code, material_id, year, unit, value, source) %>%
  arrange(alphanumiso, CCC_Code, material_id, year)

# check new entries
a1 <- setdiff(
  new_data,
  data
)

######here starts the section of union with the old detailed.rds
o <- read_rds("./04_output/02_final/DE_met_min_fos_Detailed_2021-11-04.rds")
n <- new_data#read_rds("./04_output/02_final/DE_met_min_fos_Detailed_2022-11-23.rds")
### restrict new data for years after 2015
n <- n[n$year>=2016,]
#### add 2022 update replace new years where we have the data
# n1 <- right_join(o[,1:4],n) 
old_rows <- left_join(setdiff(o[,1:4], n[,1:4]),o) # all old rows that are not in the new data
data_joined <- rbind(n,old_rows)
new_data <- data_joined

#Adjustment 2: MDV & M.sag timeseries times 10
new_data[((new_data$alphanumiso=="MDV462")+(new_data$material_id=="M.sag")+(new_data$year<2016))==3,6] <- new_data[((new_data$alphanumiso=="MDV462")+(new_data$material_id=="M.sag")+(new_data$year<2016))==3,6]*10

## save disaggregated data as rds
write_rds(
  new_data,
  paste0(
    "./04_output/02_final/DE_met_min_fos_Detailed_",
    substr(Sys.time(),1,10),
    ".rds"
  ))


## save disaggregated data as csv
write_delim(
  new_data,
  paste0(
    "./04_output/02_final/DE_met_min_fos_Detailed_",
    substr(Sys.time(),1,10),
    ".csv"
  ), 
  delim = ";", 
  na = "")


# add names and other stuff to disaggregated data
new_data <- new_data %>%
  left_join(.,
            cou_ids %>%
              select(alphanumiso, country)
  ) %>%
  left_join(.,
            ccc_vs_mat %>%
              distinct(material_id, material_name)
  ) %>%
  left_join(.,
            mfa13_ids %>%
              select(CCC_Code, CCC_Name, MFA13)
  ) %>%
  select(alphanumiso, CCC_Code, material_id, year, unit, value, source, country, CCC_Name, material_name, MFA13)



# save disaggregated data with names
write_delim(
  new_data,
  paste0(
    "./04_output/02_final/DE_met_min_fos_Detailed_with_names_",
    substr(Sys.time(),1,10),
    ".csv"
  ), 
  delim = ";", 
  na = "")













## aggregate
col_names <- names(new_data)[!names(new_data) %in% c("value", "material_id")]

data_agg <- new_data %>%
  filter(!is.na(value)) %>%
  group_by(across(all_of(col_names))) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()


# format aggregated data
data_agg <- data_agg %>%
  select(alphanumiso, CCC_Code, year, value, unit) %>%
  arrange(alphanumiso, CCC_Code, year, value, unit)




## aggregate Germany for whole time series in CCC data
  ## only Germany, because all other MFA data (i.e. biomass and all trade data) is aggregated for this country
data_agg <- data_agg %>%
  left_join(.,
            cou_for_aft %>%
              filter(after == "DEU276") %>%
              select(-category, alphanumiso = former)
            ) %>%
  mutate(
    alphanumiso = ifelse(
      is.na(after),
      alphanumiso,
      after
      )
    ) %>%
  group_by(alphanumiso, CCC_Code, year, unit) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()
  





## save CCC
write_delim(
  data_agg,
  paste0(
    "./04_output/02_final/DE_met_min_fos_CCC_",
    substr(Sys.time(),1,10),
    ".csv"
    ), 
  delim = ";", 
  na = "")




# add names and other stuff to CCC data
data_agg <- data_agg %>%
  left_join(.,
            cou_ids %>%
              select(alphanumiso, country)
  ) %>%
  left_join(.,
            mfa13_ids %>%
              select(CCC_Code, CCC_Name, MFA13)
            )

# save CCC data with names
write_delim(
  data_agg,
  paste0(
    "./04_output/02_final/DE_met_min_fos_CCC_with_names_",
    substr(Sys.time(),1,10),
    ".csv"
  ), 
  delim = ";", 
  na = "")














# ------------

# yearly sums (for checking)
a <- data_agg %>%
  group_by(year) %>%
  summarise(value = sum(value, na.rm = TRUE))

