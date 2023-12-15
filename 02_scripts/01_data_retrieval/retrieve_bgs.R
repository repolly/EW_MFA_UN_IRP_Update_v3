##### Scraping data from BGS webpage
  ### Steps:
    # Extract BGS commodity numbers from HTML code
    # Extract available years
      # Specify groups of years (max. download: 11 years per time)
    # Download of all data, in two nested loops
      # By each year group and by each commodity
        # Get commodity name
        # Get unit
        # Combine data in tibble at each end of inner loop
        # Combine data in list at each end of outer loop




library(tidyverse)
library(httr)
library(XML)

# clear R environment
rm(list = ls())




### extracting basic info from the webpage  ----
  # i.e. a list of all commodity numbers from the webpage for the further algorithm
  # by filtering the html code for specific patterns

base_URL <- "https://www2.bgs.ac.uk/mineralsuk/statistics/wms.cfc?"
URL <- paste0(base_URL, "method=searchWMS")
webpage <- httr::GET(URL)
pagetree <- XML::htmlTreeParse(webpage, useInternalNodes = TRUE)

# Parse for links
BGS_num <- xpathSApply(pagetree, "//select[@id='commodity']/option/@value")
BGS_num
BGS_num <- as.vector(BGS_num)
BGS_num <- unique(BGS_num)
BGS_num <- BGS_num[BGS_num!=""]
BGS_num <- as.integer(BGS_num)
length(BGS_num)



### extracting list of years based on webpage above and grab the latest one
  # (in order to specify ranges of years below)
year <- xpathSApply(pagetree, "//select[@id='dateTo']/option/@value")
year <- as.integer(year)
year <- year[!is.na(year)]

# set maximum of all years
max_year <- max(year)


# split all years from 1970 onwards into groups of max 10 years (because max 11 years can be downloaded at once)
year_groups <- split(year, ceiling(seq_along(year)/10))

# provide names for groups of years (name being the respective time range)
  # for formatting/cleaning afterwards
for (i in 1:length(year_groups)) {
  
  names(year_groups)[i] <- paste0(
    min(year_groups[[i]]),
    "-",
    max(year_groups[[i]])
  )
}




### retrieve all data from the website and compile it into one dataset ----

## download of all data (in loops, i.e. by each year group and by each commodity)

# create empty list in order to combine data
if(exists("bgs_list")) rm(bgs_list)
bgs_list <- list()



# loop for year groups
for (y in 1:length(year_groups)) {
  
  # define minimum and maximum year for respective year group
  min_y <- min(year_groups[[y]])
  max_y <- max(year_groups[[y]])
  
  
  # create empty tibble in order to combine data
  if(exists("BGS_data")) rm(BGS_data)
  BGS_data <- tibble()
  
  
  
  # loop for commodities
  for (i in BGS_num){
    
    # defining the URL for each commodity and unit
    
    URL <- paste0(base_URL, "method=listResults&dataType=Production&commodity=", i,
                  "&dateFrom=", min_y, "&dateTo=", max_y, "&country=&agreeToTsAndCs=agreed")
    
    # get the html code from the webpage and parse it as text
    webpage <- httr::GET(URL)
    
    webpage <- content(webpage, as = "text")
    
    pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
    
    # parse the tree for the commodity name
    Com_name <- xpathSApply(pagetree, "//*/h1", xmlValue)
    
    # if data is available, then download
    if(Com_name != "No results"){
      
      # read the tables from the webpage into R
      tables <- readHTMLTable(webpage)
      
      # bring it into a tibble
      df <- tables[1] %>% as.data.frame() %>% as_tibble()
      
      # create column with commodity name
      if(nrow(df) != 0) {
        df <- df %>%
          mutate(commodity = Com_name, .after = "NULL.V1") %>%
          mutate(bgs_num = i, .after = "commodity")
      }
      
      
      ## isolate the unit from the html code and create column with unit
        # only with most recent year (because unit is not shown for large tables)
      URL <- paste0(base_URL, "method=listResults&dataType=Production&commodity=", i,
                    "&dateFrom=", max_y,"&dateTo=", max_y, "&country=&agreeToTsAndCs=agreed")
      
      # get the html code from the webpage and parse it as text
        # needs to be done again, because here it's important to only select one year
      webpage <- httr::GET(URL)
      
      webpage <- content(webpage, as = "text")
      
      pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
      
      # parse the tree for the unit
      bgs_unit <- xpathSApply(pagetree, "//*/p[@align='right']", xmlValue) %>% 
        as.character()
      
      if(is_empty(bgs_unit)) {bgs_unit <- NA}
      
      # create column with unit
      if(nrow(df) != 0) {
        df <- df %>%
          mutate(unit = bgs_unit, .after = "commodity") %>%
          mutate(across(where(is.logical), as.character))
      }
      
      
      if(nrow(BGS_data) == 0) {
        BGS_data <- df
      } else {
        BGS_data <- BGS_data %>% union(., df)
      }
      
    }
  }

  # add data to list of tibbles for each year group and give list element the name of year group/range
  bgs_list[[y]] <- BGS_data
  
  names(bgs_list)[y] <- names(year_groups)[y]
  
}




### save rds
write_rds(
  bgs_list,
  paste0("./03_intermediate/01_data_retrieval/bgs/bgs_data_",
         max_year,"_dldate_",substr(Sys.time(),1,10),".rds")
)