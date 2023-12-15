##### Downloading commodity Excel files from USGS website
  ### Steps:
    # Get all URLs for each USGS commodity page
      # Create a list of names based on that
    # Two nested loops for downloading files
      # Outer loop to get list of latest files for each commodity
        # Filter for only the files not downloaded yet (see log file USGS_file_list)
      # Inner loop for downloading only missing files for selected commodity
    # Save updated log file USGS_file_list and save list of new files
    



library(RCurl)
library(XML)

# clear R environment
rm(list = ls())




## Isolating the URLS of each commodity webpage ----
URL <- "https://www.usgs.gov/centers/national-minerals-information-center/commodity-statistics-and-information"

webpage <- getURL(URL)

pagetree <- htmlTreeParse(webpage, useInternalNodes = TRUE)


# Parse for links
  #  if webpage changes, this has to be adjusted -> i.e. specifying a unique indicator used by all links which are needed
x <- xpathSApply(pagetree, "//*/div[@class='tex2jax_process']/ul/li/a/@href")

x <- as.vector(x, mode = "character")

x <- unique(x)


# Create a list of all USGS commodity names in/for URLs
Comlist <- substr(x,15,nchar(x))

Comlist <- Comlist[grepl("mining-and-quarrying|statistical-summary|survey-methods", Comlist) == FALSE]




## Create a list of all saved files for future checks
USGS_file_list <- data.frame("Commodity" = character(1), "File_name" = character(1), stringsAsFactors=FALSE)




## Nested loops for download of all data ----

# Outer loop to get list of latest files for commodity

for (i in Comlist[3:length(Comlist)]){
    
  # i <- Comlist[[3]]
  
  # Isolating the URLS of each mineral yearbook excel file
  URL <- paste0("https://www.usgs.gov/centers/national-minerals-information-center/", i)
  
  webpage <- getURL(URL)
 
  pagetree <- htmlTreeParse(webpage, useInternalNodes = TRUE)
  
  # Parse for links
  y <- xpathSApply(pagetree, "//li/a/@href")
  
  y <- as.vector(y, mode = "character")
  
  y <- unique(y)
  
  ###Get only xls and xlsx
  yxlsx <- grep('xls|xlsx', y, value=T)
  
  yxlsx   #List of xls files
  
  # grab only data more recent than 2013
  year <- as.integer(substr(Sys.time(),1,4))
  
  year <- 2016:year
  
  # create a dynamic search pattern for grep to only include years > 2013
  year <- paste0(as.character(year), collapse="|")
  
  
  # Get only latest years (as it doesn't make sense to download all Excel files again)
  ylatest <- grep(year, yxlsx, value=T)
  
  ylatest   # List of most recent files 
  
  
  # Exclude industry surveys
  ymyb <- ylatest[grepl("mis-", ylatest)==FALSE]
  
  ymyb # List of only yearbooks 
  
  
  # grab only the files not downloaded yet
    # grab the ones already downloaded
    # the function takes the last modified file in a directory with a specific pattern
  source("./02_scripts/00_functions/lastfile.R")
  
  last_dl <- my_lastfile("./01_input/02_log_files/usgs_retrieval/", "USGS_file")
  
  last_dl_df <- as.data.frame(read.csv(last_dl))
  
  
  # keep only files not downloaded yet
  missing <- which((basename(ymyb) %in% last_dl_df[,1]) == FALSE)
  
  # take the index and apply it to the list with the download links
  ymyb <- ymyb[missing]
  
  
  
  ## Inner loop for downloading only missing files
  for (j in ymyb) {
    
    if (length(ymyb) == 0) {break}
    
        # Defining the destination file for each commodity
        Dest <- paste0("./03_intermediate/01_data_retrieval/USGS/",  basename(j)) # substr(j, nchar(j)-19+1, nchar(j))
            
        # Download and save the file
        download.file(paste0(j), destfile = Dest, mode = "wb")
    
        # Append list of all saved files for future checks
        USGS_file_list <- rbind(USGS_file_list, c(substr(i,1,nchar(i)-1),basename(j)))
        
  }
  
  }



# stick new USGS_file_list together with old one
USGS_file_list_old <- data.frame(read.csv(last_dl, stringsAsFactors = FALSE))

USGS_file_list_new <- USGS_file_list


# remove empty rows initially needed to create the data frame
USGS_file_list_new <- USGS_file_list_new[!apply(USGS_file_list_new == "", 1, all),]

write.csv(USGS_file_list_new, paste0("./01_input/02_log_files/usgs_retrieval/USGS_updated_cases_",substr(Sys.time(),1,10),".csv"), row.names = FALSE)

# this could be a possibility to mark the newer values
USGS_file_list_updated <- rbind(USGS_file_list_old, USGS_file_list_new) %>% arrange(Commodity)

USGS_file_list_updated <- USGS_file_list_updated %>% distinct(File_name)


#colnames(USGS_file_list) <- c("Commodity", "File_name")
write.csv(USGS_file_list_updated, paste0("./01_input/02_log_files/usgs_retrieval/USGS_file_list_",substr(Sys.time(),1,10),".csv"), row.names = FALSE)


#TODOXX at the moment there are duplicates in the USGS_file_list, erase them, either with distinct or before they even start 


