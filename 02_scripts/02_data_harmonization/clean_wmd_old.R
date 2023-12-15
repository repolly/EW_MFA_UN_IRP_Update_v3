##### Format all datasets from WMD
  ### Steps:
    # Format recently downloaded data (i.e. for five latest years)
      # Read all individual spreadsheets
      # Delete obsolete rows
      # Turn into long format




#### load packages
library(tidyverse)
library(readxl)

# clear R environment
rm(list = ls())




#### clean wmd recently downloaded data --------------------------------------

## read data

# function to get the last modified file in a directory with a certain pattern in it
source("./02_scripts/00_functions/lastfile.R")

# get the file path of the latest file with a given pattern
lastfile <- my_lastfile("./03_intermediate/01_data_retrieval/wmd/", "wmd_raw")
path <- lastfile


# read in excel file and combine all worksheets (documentary here: https://readxl.tidyverse.org/articles/articles/readxl-workflows.html)
type_list <- c(rep("text", 2), rep("numeric", 5), "text")

wmd <- path %>%
  excel_sheets() %>%
  set_names() %>% 
  map_dfr(
    ~ read_excel(
      path = path, 
      sheet = .x, 
      col_types = type_list,
      skip = 1,
      na = c("", "-", "NA")), 
    .id = "sheet"
    )

# path <- "./03_intermediate/01_data_retrieval/wmd/wmd_raw_2021-04-07.xlsx"

#### add only the 2020 column
# read in excel file and combine all worksheets (documentary here: https://readxl.tidyverse.org/articles/articles/readxl-workflows.html)
# wmd2015 <- path %>%
#   excel_sheets() %>%
#   set_names() %>% 
#   map_dfr(
#     ~ read_excel(
#       path = path, 
#       sheet = .x, 
#       col_types = type_list,
#       skip = 1,
#       na = c("", "-", "NA")), 
#     .id = "sheet"
#   )

#### add 2022 update
# wmd1 <- right_join(wmd2015[,1:4],wmd) # add 2015 column to the new data
# old_rows <- left_join(setdiff(wmd2015[,1:3], wmd[,1:3]),wmd2015) # all old rows that are not in the new data
# old_rows$'2020' <- NA #  add 2020 col where we do not know the number
# old_rows <- old_rows[,c(1:8,10,9)] # change order so taht 2020 is not the last col
# # wmd <- rbind(wmd1,old_rows)# combine these old rows with the wmd1 set 
# nrow(wmd)

### cleaning data

# delete every row that has the following string in it
  # (in case it has not already been skipped, as should be the case)
wmd <- wmd[!grepl("6.4. Production of Mineral Raw Materials of individual", wmd$Country),]

# delete every row that has the following string in it
  # which should not be the case as column names are taken from excel sheets
wmd <- wmd[!grepl("Country", wmd$Country),]

# delete every row that has the following string in it
wmd <- wmd[!grepl("Total", wmd$Country),]



# get the highest year covered in the most recent file
year_high <- max(as.integer(colnames(wmd)), na.rm = TRUE)

# count down 4 years, because usually always 5 years are available for WMD
year_low <- year_high - 4 

# adjust column names
wmd <- wmd %>% rename("commodity" = "sheet", 
                      "country"   = "Country",  
                      "source"    = "data source")



### convert from wide to long based on available years
years <- (as.character(year_high:year_low))

wmd <- wmd %>%
  gather(
    key = "year",
    value = "value",
    all_of(years))

# delete values filled in at the source column for the years lower than the highest year, because they are only valid for the highest year
years_norep <- (as.character(year_high:year_low-1))
wmd <- wmd %>% mutate(source = ifelse(year %in% years_norep, NA, source))


### save cleaned table of most recent data
write_rds(wmd, file = "./03_intermediate/02_data_harmonization/clean_wmd_rec.rds")










#### WMD data for 2015-2017 with footnotes on sources -----------------------
wmd <- readxl::read_excel("./01_input/04_former_data/wmd/6.4. acc source.xlsx", col_names = TRUE)

wmd <- wmd[-c(1),-c(4,7,10,11:13)]

names(wmd) <- c("country","2015","2015_acc","2016","2016_acc","2017","2017_acc")



# column 2015_acc usually has NAs in the column where the commodity is listed
  # therefore grab the row index and display the first column

com <- which(is.na(wmd$`2015_acc`))

# check table manually for missings, because there are a few cases where NAs are in the same row as the countries
check_com <- wmd[com,1]

# exclude these NAs from table, the rest should be the row name of commodities
com <- com[-c(5, 8, 10, 23, 28, 32, 38, 47, 55, 56)]

# grab all commodity names
com_names <- wmd[com,1]

# fill in the commodity names at the right place
wmd[com,"commodity"] <- com_names


# loop through the column and fill with the value before
for (i in 1:nrow(wmd)) {
  
  tryCatch({

  if (is.na(wmd$commodity[i+1])) {
    
    wmd$commodity[i+1] <- wmd$commodity[i]
    
    }
    
  }, error=function(e){}) 
    
}

# delete original commodity rows, because they only contain the totals, which are not needed
wmd <- wmd[-com,]

# convert to long format
col_names <- c("2015", "2016", "2017")

wmd <- wmd %>%
  select(country, !!col_names, commodity) %>%
  pivot_longer(
    c(!!col_names),
    names_to = "year",
    values_to = "value") %>%
  left_join(.,
            wmd %>%
              select(-!!col_names) %>%
              rename(`2015` = `2015_acc`, `2016` = `2016_acc`, `2017` = `2017_acc`) %>%
              pivot_longer(
                c(!!col_names),
                names_to = "year",
                values_to = "rep")
    
  )



# rename values 
wmd_rep <- wmd %>% 
  mutate(rep = ifelse(rep == 1, "r", "e"))



write_rds(wmd_rep, file = "./03_intermediate/02_data_harmonization/clean_wmd_reported_15_17.rds")




