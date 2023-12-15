### Cleaning of recently retrieved (i.e. downloaded) USGS data

# Checking the USGS_updated_cases file from retrieval script to "know" which 
# excels the cleaning script should take into account
# Respective data is cleaned and combined into one data frame
# Please note: USGS data is comparatively messy, so it would be no surprise if errors occur in the future.
# unfortunately this can´t be evaded due to so much inconsistency in the original data files.


########### Not sure yet if all commodities are included in the final data, because there are a couple of
########### warnings saying "Data frame contains 0 rows and 0 columns!"
########### (However, the relevant commodities are included)


########## Also, formatting is not finished yet for sub-commodities included in the same column as country names
########## (However, that's not a problem, because it's not the case for those commodities derived by us from USGS
##########  except for "Peat, horticultural use" and "Pumice" which has been formatted accordingly)


######### USGS Country List always has to be updated. If not, then there might be errors introduced at some point
######### (in particular in the data on pumice production)




# Get right Excel sheets and integrate them in one document




library(tidyverse)
library(readxl)

# clear R environment
rm(list = ls())




### read files


## lists
  ## (of commodities with all denominations, of USGS country names, of downloaded files)
    
    ## Concordance list of denominations by USGS includes all kinds of denominations (web, Excel file, Excel sheet)
    ## (they'll unlikely change and if they do then it will probably be just a few cases)
    
    ## Country list is needed to clean the country names further down from cross-references and stuff

USGS_Comlist <- as.data.frame(read.delim("./01_input/01_concordance_tables/usgs_retrieval/USGS_commodity_list.txt", header = TRUE,
                                         quote = "", row.names = NULL, stringsAsFactors = FALSE))

USGS_Coulist <- as.data.frame(read.delim("./01_input/01_concordance_tables/usgs_retrieval/USGS_country_list.txt", header = TRUE, 
                                         quote = "", row.names = NULL, stringsAsFactors = FALSE))

USGS_Coulist[USGS_Coulist$USGS.Country == "Côte d'Ivoire (Ivory Coast)","USGS.Country"] <- "Côte d’Ivoire" # changed name


USGS_Unitlist <- as.data.frame(read.delim("./01_input/01_concordance_tables/usgs_retrieval/USGS_unit_list.txt", header = TRUE, 
                                         quote = "", row.names = NULL, stringsAsFactors = FALSE))


# latest downloaded files

  # source("./02_scripts/00_functions/lastfile.R")
  # 
  # lastfile <- my_lastfile("./01_input/02_log_files/usgs_retrieval/", "USGS_file_list_")

USGS_Downl <- list.files("./03_intermediate/01_data_retrieval/USGS/")

USGS_Downl <- USGS_Downl[USGS_Downl != "extension"]

# USGS_Downl <- grep("cemen", USGS_Downl, value = TRUE)

USGS_Downl <- as.data.frame(USGS_Downl)

colnames(USGS_Downl)[1] <- "File_name"





## Setting variables

# time range of downloaded data
min <- min(as.integer(substr(USGS_Downl[,1],6,9)), na.rm = TRUE)
max <- max(as.integer(substr(USGS_Downl[,1],6,9)), na.rm = TRUE)

yearlist <- min:max
n_done <- vector()
cou_list <- USGS_Coulist[,1]

all_data <- data.frame()


### Expand the USGS download list to two more columns which include the Excel file denomination
### in order to have unique denominations for the loop
### (e.g. issue with magnesium and magnesite both having the web-URL denomination "magnesium", but
### both have different denominations for their excel files which therefore provides a unique separation)

USGS_Downl[,2] <- gsub('-adv.xlsx|-adv.xls|-advrel|.xlsx|.xls',"", 
                       substr(USGS_Downl[,1], 11, 100))

colnames(USGS_Downl)[2] <- "Exc_name"

### Filter for those commodities which we are interested in  
                                    ##### THESE MIGHT CHANGE IN THE FUTURE, FOR EXAMPLE WITH NEW DATA
  ### Includes all those from the USGS_Comlist

USGS_filel <- USGS_Downl[USGS_Downl[,2] %in% USGS_Comlist[,4],]

### Filter for unique files (in case of existing doubles)
USGS_filel <- unique(USGS_filel)


# I excluded some cases where there occured problems
# In 99% of the errors the naming of the excel headline has changed (within the worksheet)
# So we can grab the excel as object sh1, but we can´t grab the right worksheet from within
# because the name has changed, so no sh2 and therefore no further calculations possible
USGS_filel <- USGS_filel %>% 
  filter(
    !File_name %in% c(
      "myb1-2018-bismu-adv.xlsx",
      "myb1-2017-bismu.xls",
      "myb1-2017-bismu-adv.xlsx",
      "myb1-2016-bismu.xlsx", 
      "myb1-2015-bismu.xlsx",
      "myb1-2014-diamo.xlsx",
      "myb1-2017-diamo-adv.xlsx",
      "myb1-2014-gemst.xlsx",
      "myb1-2018-kyani-adv.xlsx",
      "myb1-2017-kyani.xls",
      "myb1-2016-kyani.xlsx",
      "myb1-2015-kyani.xlsx",
      "myb1-2018-niobi-adv.xlsx",
      "myb1-2017-niobi.xls",
      "myb1-2016-niobi.xlsx",
      "myb1-2015-niobi.xlsx",
      "myb1-2018-plati-adv.xlsx",
      "myb1-2018-silic-adv.xlsx",
      "myb1-2017-silic.xls",
      "myb1-2016-silic.xls",
      "myb1-2015-silic.xls",
      "myb1-2015-perli.xlsx",
      "myb1-2015-indiu.xls",
      "myb1-2017-salt-adv.xlsx",
      "myb1-2016-salt-adv.xlsx", # problem in the if statement
      "myb1-2015-salt.xls", # problem in the if statement
      "myb1-2017-selen.xls",
      "myb1-2016-diato.xlsx",##here start the new files that produce errors
      "myb1-2017-diato.xlsx",
      "myb1-2018-diato.xlsx",
      "myb1-2017-diamo.xls",
      "myb1-2017-gemst.xls",
      "myb1-2016-silic.xlsx",
      "myb1-2017-silic.xlsx",
      "myb1-2018-silic.xlsx",
      "myb1-2019-silic.xlsx",
      "myb1-2018-fluor-adv.xlsx",
      "myb1-2018-plati.xls",
      "myb1-2018-salt-adv.xlsx",
      "myb1-2019-arsen-adv.xlsx",
      "myb1-2019-bismu-adv.xlsx",
      "myb1-2019-diamo-adv.xlsx",
      "myb1-2019-fluor-adv.xlsx",
      "myb1-2019-kyani-adv.xlsx",
      "myb1-2019-niobi-adv.xlsx",
      "myb1-2019-silic-adv.xlsx",
      "myb1-2020-bismu-adv.xlsx",
      "myb1-2020-niobi-adv.xlsx",
      "myb1-2020-selen-adv.xlsx"
      ))
# File_name



### Beginning of first loop to select each Excel file for a commodity in the list and then
### do the data extraction process

##### First Loop : Years #####

for (i1 in yearlist[yearlist>2015]) {

## Get the commodities from the years that have been gone through already
## and create a list of those commodities from the current year which have not been processed yet for the other years

  
 # i1 <- 2016
  
prev_y <- i1+1

r_done <- grep(prev_y, USGS_filel[,2], value = F) # grab all with year 2015

n_done <- vector()
n_done <- c(n_done, as.character(USGS_filel$Exc_name[r_done])) # grab all Excel commodity names from downloaded files which were processed, EXCEL NAMES

USGS_Downl_nd <- USGS_filel[!(USGS_filel$Exc_name %in% n_done),] # grab all Excel commodities which where not done in 2015 for year 2014/ 2013, EXCEL NAMES

openlist <- USGS_Downl_nd[grep(i1, USGS_Downl_nd[,1], value = F),] # grab all commodities which were not done in the i+1 year (2015) and check in the prev year (2014)
openlist$File_name <- as.character(openlist$File_name)

print(i1)


# browser()

##openlist has 2014 file list for commodities that where not processed for 2015
##e.g. bauxi wasn´t done for 2015, so take it from 2014
##e.g. alumi was done for 2013 - 2014 - 2015, so don´t include it

##### Second Loop : create a list for commodities #####

### Open each Excel from the list, look for the right data, extract and format it, and add it to an overall dataframe
for (i2 in openlist[,2]){
  
  #i2 <- grep("chrom", openlist$Exc_name)
  #i2 <-  openlist$File_name[i2]
  
  #i2 <- openlist[3,2]

  
### Short version of getting right to the respective data
  ###(longer version copied to the annex of this code)
  ### Take the file name and select the respective commodity web denomination 
  ### from openlist and then based on that make a list of the different production tables
  ### contained in that file, i.e. the USGS Denominations of production from USGS_Comlist
  # grab the production name for the corresponding loaded excel file (e.g. I find out 2015 bauxit is missing, grab 2014 bauxit and now search for the production name of baucit) 
  Prod_name <- USGS_Comlist$USGS.Denomination.of.production[USGS_Comlist$Commodity.Excel.file.denomination == i2]
  
  # Filter those which create errors right now
  Prod_name <- Prod_name[!(Prod_name %in% "FULLER'S EARTH: WORLD PRODUCTION")]
  
  File_name <- openlist %>%
    filter(Exc_name == i2) %>%
    select(File_name) %>%
    pull()
  
  ### Open the Excel file
  path <- paste0("./03_intermediate/01_data_retrieval/USGS/", File_name)
  
  shdata1 <- lapply(excel_sheets(path), read_excel, path = path)
  
  
  
##### 3rd LOOP #####
  
  ### Do a little loop in order to extract and format data for each relevant data table
  ### in the excel file separately (e.g. necessary for Selenium incl. Selenium and Tellurium)
  for (i2.5 in Prod_name) {
    
   #i2.5 <- Prod_name[1]
    
    ## Creating a flexibel denomination (in case of unexpected spaces within the commodity name
      ## Just entering a ".*" in the middle in case of additional spaces
      ## Also getting rid of "Estimated" and anything in brackets, because they cause problems
    Flex_prodn <- paste0(gsub(":.*", "", 
                         gsub("//(.*)", "", i2.5)),".*", 
                         gsub(".*: ", "", 
                         gsub("ESTIMATED ", "", i2.5)))
    
    shdata2 <- shdata1[grep(Flex_prodn, shdata1, value=F)]
    # if(i1==2016 && i2==silic && )shdata1[[11]][,1]
    
    
    df1 <- as.data.frame(shdata2)


### Formatting the data frame
  ## The final result should just be a frame with a column for commodity name, country names and one for units
  ## as well as a head row for years, and then of course the values

    
##### Formatting #####
    
  ## Getting rid of unnecessary columns and rows
df2 <- df1[, -c(2)]

#special case for IRON, because it has two commodities next to each other

if (i2.5 == "IRON ORE: WORLD PRODUCTION") {
  df2 <- df2[, -c(12)]# df1[, colSums(is.na(df1)) != nrow(df1)] is a dynamic solution to kick all columns with only NAs in it, but could be problematic due to footnotes
} else{}
    
    

lr1 <- df2[grep('eEstimated|rRevised|W Withheld|Estimated|Preliminary', df2[,1]),]
lr2 <- rownames(lr1)
lr3 <- as.integer(lr2)-1
# browser()

#lr1 <- df2[grep("Total", df2[,1]),]
#lr2 <- lr1[grep("Totals", lr1[,1], invert = TRUE),]
df3 <- df2[1:lr3,]


  ## Setting commodity and unit
USGSCom <- df3[1,1]

commodity <- sub(":.*|//..*|,.*", "", USGSCom)

unit <- df3[3,1]

  ## Columns for commodity and country / as well as column names
df4 <- data.frame(commodity, unit, df3)
df4$commodity <- as.character(df4$commodity)
df4$unit <- as.character(df4$unit)

# special case: Natural Diamond, because it has Gemstone and Industrial 
if (commodity == "NATURAL DIAMOND") {
  start <- grep("Gemstones:",df4[,3]) + 1
  end <- grep("Industrial:",df4[,3]) - 2
  
  df4[start:end,1] <- "Natural Diamonds Gemstone"
  
  start <- grep("Industrial:",df4[,3]) + 1
  end <- nrow(df4) - 2 # because of total and grand total at the end
  
  df4[start:end,1] <- "Natural Diamonds Industrial"
} else{}





######NUMBERING MAX

# Select only those columns where footnotes are in (e.g. reported), so every 2nd (+ the ones never chaning like country etc.)
colnr_rep <- vector()
length(colnr_rep) <- floor((ncol(df4)+3)/2)
colnr_rep[1:3] <- c(1:3)

for (i in 4:length(colnr_rep)) {
  colnr_rep[i] <- (2*i-3)
}


#first: Give fixed columns a name
colnames(df4)[1:3] <- c("commodity", "unit", "country")

# index for footnote columns and year columns
colnr_rep
colnr_rep <- colnr_rep[-c(1:3)]

#create name plates
colnames_rep <- paste0(as.character((i1-4):i1),"_rep") # problem: there is no reported column for 2016, but this is apparently okay just that last number is lost (which is good)

# use name plate on the colnumber index to replace the column names, error that replacement length isn´t always the same is okay, because not always is there a footnote
colnames(df4)[colnr_rep] <- c(colnames_rep)

### Now search column numbers for year columns
#select columns where years are in 

colnr_y <- colnr_rep-1

if (ncol(df4) == 12) { 
  colnr_y <- colnr_rep - 1
  colnr_y <- append(colnr_y, 12)  # add the column index for the last column 12
} else if (ncol(df4) == 22) { #same could happen for e.g. IRON ORE if there was no footnote in one column
  colnr_y <- colnr_rep - 1
  colnr_y <- append(colnr_y, 22)
} else {
  colnr_y <- colnr_rep - 1
}

#4 because the data starts after this

#use name plate on the colnumber index to replace the column names
colnames(df4)[colnr_y] <- as.character((i1-4):i1)


## Deleting first empty rows
fr <- as.integer(rownames(df4[grep('Country|country', df4[,3]),]))+1
df5 <- df4[fr[1]:nrow(df4), ]

df5d <- df5
df5[,colnr_y] <- as.numeric(gsub("//.", "", as.matrix(df5[,colnr_y])))


### Replacing "." for "" in number values for the year columns and turning values into numeric values
### (which at the same time removes the "--" and turns it into NA)



### Formatting country names

  ### Differentiating if Peat or Pumice production
  ### (More effort on formatting only necessary for peat and pumice at the moment, 
  ### because the rest of data doesn't need it or is retrieved from BGS)




##### IF CONDITIONS FOR SPECIAL CASES #####

##hard code bosnia and herzegovina

if(i2.5 == "IRON ORE: WORLD PRODUCTION" & i2 == "myb1-2015-feore.xlsx" ) {
  #Hardcode Bosnia and Heregovina 
  df5[grep("Bosnia and", df5$country),"country"] <- "Bosnia and Herzegovina"
}


# hard code czechia -> Czech Republic in this one excel file, because usually it was always Czech Republic
if(i2 == "myb1-2016-felds-adv.xlsx") {
  #Hardcode Bosnia and Heregovina 
  df5[grep("Czechia", df5$country, fixed = TRUE),"country"] <- "Czech Republic"
} else{}


# Special cases to clean up before further calculations

if (commodity == "SALT") {
  
  #search for the "continued" Table in Country column (hopefully this is unique)
  mid <- grep("Continued", df5$country)
  start <- mid - 1
  end <- mid + 5
  kick <- vector()
  
  ### add up all the values that should be kicked in an empty vector
  for (j in seq_along(start)) {
  
  bet <- c(start[j]:end[j])
  kick <- append(kick, bet)
  
  }
  
  # delete the values
  df5 <- df5[-c(kick),]
  
  rm(mid,start,end,kick,bet)
} else{}

###Special case: IRON ORE, because of messy data for China, differs in every excel sheet, so better look it up. Might be cleaner in the future though
# NOTE: Has to happen before country names get cleaned
if (commodity == "IRON ORE" & i2 == "myb1-2015-feore.xlsx") {
  
  #take the Crude ore column
  crude <- grep("*rude", df5$country)
  china <- crude - 1
  usable <- crude + 1
  
  #put crude ore to china
  df5[china, colnr_y[1:5]] <- df5[crude,colnr_y[1:5]] # numbers
  df5[china, colnr_rep[1:5]] <- df5[crude,colnr_rep[1:5]] # footnotes
  
  # put usable or to china 
  df5[china, colnr_y[6:10]] <- df5[usable,colnr_y[6:10]] #numbers
  df5[china, colnr_rep[6:10]] <- df5[usable,colnr_rep[6:10]] #footnotes
  
  # delete the used columns for crude and usable ore
  df5[c(crude,usable),] <- NA
  
  
} else if (commodity == "IRON ORE" & i2 == "myb1-2014-feore.xlsx") {
  
  china <- grep("*rude", df5$country) # This row will be the china row after getting cleaned up, we just put usable or in the same row as china and crude ore
  usable <- china + 1
  
  # put usable ore to china 
  df5[china, colnr_y[6:10]] <- df5[usable,colnr_y[6:10]] #numbers
  df5[china, colnr_rep[6:10]] <- df5[usable,colnr_rep[6:10]] #footnotes
  
  # not delete usable to avoid double counting, because after name cleaning it would say just "china"
  df5[usable,] <- NA
} else{}
rm(china, usable, crude)


# Special Commodities -----------------------------------------------------

special_com <- c("FLUORSPAR","PUMICE AND RELATED MATERIALS","KYANITE AND RELATED MINERALS", "CHROMITE", "SALT")

## For peat
if (commodity == "PEAT") {
  
  for (i3 in 1:nrow(df5)) {
    
    for (i4 in 0:60) { 
      
      if (substr(df5$country[i3],1,nchar(df5$country[i3])-i4) %in% cou_list 
          && grepl("horticultural", df5$country[i3]) 
          && ! grepl("fuel", df5$country[i3])) { 
        c(df5$country[i3] <- cou_list[match(substr(df5$country[i3],1,nchar(df5$country[i3])-i4), cou_list)], break)
        
      } else if (substr(df5$country[i3],1,nchar(df5$country[i3])-i4) %in% cou_list 
                 && grepl("Horticultural", df5$country[i3+1])) {
        c(df5$country[i3] <- cou_list[match(substr(df5$country[i3],1,nchar(df5$country[i3])-i4), cou_list)],
          df5[i3, colnr_y] <- df5[i3+1, colnr_y],
          df5[i3+1, ] <- NA,
          break)
      } else if (grepl(".*uel use", df5$country[i3]) || grepl(".*Total|.*total", df5$country[i3])) {
        c(df5[i3, colnr_y] <- NA, break)
      } else {}
      
    }
    
    if (identical(df5$country[i3], df5d$country[i3])) {
      df5[i3, ] <- NA
    } else {}
  }  
  
  

### If not match for the country name, then add values to the country name above

} else if (commodity %in% special_com) {
  
  for (i3 in 1:nrow(df5)) {
    
    for (i4 in 0:60) {
      
      
      if (substr(df5$country[i3],1,nchar(df5$country[i3])-i4) %in% cou_list) {
        c(df5$country[i3] <- cou_list[match(substr(df5$country[i3],1,nchar(df5$country[i3])-i4), cou_list)], break)
        ### Das kann ich so nicht machen, weil wenn im ersten Loop -i4 nicht passt,
        ### dann kommt ja gleich die n?chste if-Bedingung und die macht dann schei?.
        ### Also sollte die zweite Bedingung in einem extra Loop passieren, wenn alle L?ndernamen bereinigt sind
        
      } else {}
    }
  } 

  ##### Second Loop: Check whether there is a total somewhere, if is exists, take it, otherwise take whatever is
  # in between two country names
  
  for (i3 in 1:nrow(df5)) {
    
    
    if (!(df5$country[i3] %in% cou_list))  {    #&& grepl("*yanite|*illimanite", df5$country[i3]))
      
      # first appearance of no country
      start <- i3
      
      #next appearance of a country (in between should be space to sum up)
      all <- match(cou_list, df5$country)
      all <- all[!is.na(all)]
      
      next_cname <-  min(all[all > i3]) # smallest match that is bigger than the start
      
      if (is.na(next_cname) | next_cname == Inf) {break}
      
      #is there a total in between?
      totals <- grep("Total", df5$country)
      totals_bet <- between(totals, start, next_cname)
      
      #is the total in between start and the next country name?
      
            if (any(totals_bet == TRUE)) { # if TRUE, take the total, if not true, sum up everything between # !is.na(totals[which(totals_bet == TRUE)])
              
              total_bet <- totals[which(totals_bet == TRUE)]
              df5[i3-1, colnr_y] <- df5[total_bet, colnr_y] # overwrite the column with a country name with the totals
              df5[i3-1, colnr_rep] <- df5[i3, colnr_rep] # take the footnotes from i3 to the "real" country row
              
              # set all the values "on the way" to NA to avoid double counting
              df5[start:(next_cname-1), c(colnr_y,colnr_rep)] <- NA
      
            }
            
                  else if (totals_bet == FALSE) {
                    
                    df5[i3-1, colnr_y]  <-   colSums(df5[start:(next_cname-1), colnr_y])
                    df5[i3-1, colnr_rep] <- df5[i3, colnr_rep] # take the footnotes from i3 to the "real" country row
                    
                    df5[start:(next_cname-1), c(colnr_y,colnr_rep)] <- NA # set the taken values to NA
        
        
      }
    }
  }
  
} else {

  for (i3 in 1:nrow(df5)) {
    
    for (i4 in 0:60) { 
      
      if (substr(df5$country[i3],1,nchar(df5$country[i3])-i4) %in% cou_list) 
      { c(df5$country[i3] <- cou_list[match(substr(df5$country[i3],1,nchar(df5$country[i3])-i4), cou_list)], break) }
      ## And it is breaking the loop after it has found a result, 
      ## because otherwise 'Nigeria' would be turned into 'Niger' two loops later
      
    }
    
  }
  
}

### Getting rid of the "total" rows and "Other"

df6 <- df5[grep("Totale|Other|Total|Grand total|Zero", df5$country, invert = T), ]

df6[,colnr_rep] <- lapply(df6[,colnr_rep], as.character) # not sure why this is neccessary, but sometime the data types of the reported columns come out weird, so this is safer

### cut out data if it is bigger than usual (e.g. "myb1-2016-feore-adv.xlsx") has 2 iron production fields
### so cut it out after the biggest possible reported column index, which should be 13 (3 commodity, unit, country + 5 years + 5 reported values (max))
### could also be 12 depending on whether there were reports for every year
### question: How to be sure that everytime the last year is not reported... has to be checked for a system....


### Special case IRON ORE: Two commodities in one excel sheet
if (commodity == "IRON ORE") {
  # split the relevant columns (year columns (cols_y) and footnote columns (cols_rep)) in two parts, should be 5 all the time actually 
  cols_y <- length(colnr_y)/2
  cols_rep <- length(colnr_rep)/2
  
  #define range to take
  cols_y <- colnr_y[1:cols_y]
  cols_rep <- colnr_rep[1:cols_rep]
  
  #create first dataframe with first commodities
  first <- df6[,c(1:3,cols_y,cols_rep)]
  first <- first %>% mutate(commodity = ifelse(commodity == "IRON ORE", "IRON ORE GROSS WEIGHT (CRUDE ORE)", commodity)) 
  
  #now define all columns that were not included before and do the same procedure
  cols_y <- setdiff(colnr_y,cols_y)
  cols_rep <- setdiff(colnr_rep,cols_rep)
  second <-  df6[,c(1:3,cols_y,cols_rep)]
  second <- second %>% mutate(commodity = ifelse(commodity == "IRON ORE", "IRON ORE IRON CONTENT (USABLE ORE)", commodity)) 
  
  #bind data frames together and overwrite df6
  df6 <- bind_rows(first,second)
  rm(first,second,cols_y,cols_rep)
  
}

if (ncol(df4) >= 13){
    df6 <- df6[,1:13] 
  } else {
    df6 <- df6[,1:12] # this assumes that maximum one "reported" columns is missing, see comment before. If there are cases                      # where more than one column is missing this has to be adjusted ###: QUESTION
  }


### Adding data to an overall dataframe

all_data <- bind_rows(all_data, df6)

print(unique(df6$commodity))

#all_data <- rbind(all_data, df5)

    }
}
}

### Bringing year columns into right order
#all_data_ordered <- all_data[,c(1:3,9,4:8)]

##### Finish of adjustment #####


##this is supposed to be a check file whether there were some numbers not processed
##whatever file has no countryname in it will pop up, the ones with numbers for the years
## are the most interesting cases, because this means that somehow numbers were not included
usgs_check <- all_data %>% filter(!country %in% cou_list)
write.csv(usgs_check, file = "./01_input/02_log_files/usgs_check.csv")



###Just for now to test implementation
usgs <- all_data %>% filter(country %in% cou_list)

###order usgs data, so convertion to long format works out
usgs <- usgs[ ,sort(names(usgs))]

###bring it to long format
#first: create a new index for every column with "_rep" in it
#second: index for the year columns
cols_rep <- grep("_rep", colnames(usgs), value=TRUE)
cols_y <- grep("\\b\\d{4}\\b", colnames(usgs), value=TRUE) #take any digits and create a boundary for them


# wide -> long
usgs <- data.table::melt(data.table::setDT(usgs),
                         id.vars = c("commodity", "unit", "country"),
                         measure.vars=list(c(cols_y), c(cols_rep)),
                         variable.name='year', value.name=c('value', 'rep'))

#formatting and names for year column
usgs$year <- as.integer(usgs$year)
usgs$year <- usgs$year + as.integer(min(cols_y))-1
usgs[,c("commodity", "unit", "country")] <- lapply(usgs[,c("commodity", "unit", "country")], as.character)
usgs$value <- as.numeric(usgs$value)
#third: just the first 3 columns(should stay the same all the time)

### check for duplicates
usgs %>% group_by(commodity, country, year) %>% summarize(n = n()) %>% filter(n > 1) %>% ungroup()

# erase 100% duplicates
usgs <- usgs %>% distinct(commodity, unit, country, year, value)

# if somehow two values exist for a country - commodity - year combination, just take the higher value
# I guess one could assume that these are the updates values for a country
# although this shouldn´t happen I guess this is a good solution up to the point this
# can be fixed before even arising, but for now it is important that we don´t include duplicates
usgs <- usgs %>%
  group_by(commodity, unit, country, year) %>%
  top_n(1, abs(value))

usgs %>% group_by(commodity, unit, country, year) %>% summarize(n = n()) %>% filter(n > 1)


write_rds(usgs, file = "./03_intermediate/02_data_harmonization/clean_usgs.rds")
