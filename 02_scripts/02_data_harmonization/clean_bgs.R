##### Format recently downloaded data from BGS (i.e. for latest years)
  ### Steps:
    # Define years for column names in wide format
    # Provide names to production columns
    # Provide names to footnote columns
    # Turn to long format
    # Format column types and NAs





library(tidyverse)

# clear R environment
rm(list = ls())




# read data

# get the last modified file from bgs sub-folder
source("./02_scripts/00_functions/lastfile.R")
lastfile <- my_lastfile("./03_intermediate/01_data_retrieval/bgs/", "bgs_data")

bgs_list <- read_rds(lastfile)


# empty tibble for combining data
all_data <- tibble()



## cleaning in loop for each year group of 10 years
for (y in 1:length(bgs_list)) {
  
  #y <- 3
  
  # tibble from list (minus bgs_num, because only needed for checking after data retrieval)
  bgs_data <- bgs_list[[y]] %>%
    select(-bgs_num)

  # years for column names
  min_y <- names(bgs_list)[y] %>% substr(., 1, 4)
  
  max_y <- names(bgs_list)[y] %>% substr(., 6, 9)
  
  years <- min_y:max_y
  



# define column numbers for everything not being footnotes
  # (first four and then every second one)
colnr <- vector()

length(colnr) <- (ncol(bgs_data)+4)/2

colnr[1:4] <- c(1:4) # no data in these columns

for (i in 5:length(colnr)) {
  colnr[i] <- (2*i-4)
}

# define column numbers for footnote columns
  # only taking into account everything starting from column 5
colnr_fn <- colnr[5:length(colnr)] - 1


# provide column names to table (for everything except footnotes)
colnames(bgs_data)[colnr] <- c("country", "commodity", "unit", "sub_commodity", years)

# define available years (dynamic, in order to be suitable for different time ranges)
colnames_y <- as.numeric(colnames(bgs_data))

colnames_y <- (min(colnames_y, na.rm = TRUE):max(colnames_y, na.rm = TRUE))

colnames_y <- as.character(colnames_y)

# create name templates 
colnames_fn <- paste0("fn_", colnames_y)

# provide names to footnote columns
colnames(bgs_data)[colnr_fn] <- colnames_fn



# turn data into long format
  # first exclude footnote columns, then switch years/values to long format,
  # then left join with table where footnotes have been switched to long format
bgs_data <- bgs_data %>%
  select(-all_of(colnames_fn)) %>%
  pivot_longer(
    all_of(colnames_y),
    names_to = "year",
    values_to = "value"
    ) %>%
  left_join(.,
    bgs_data %>%
      select(-all_of(colnames_y)) %>%
      rename_with(~ colnames_y[which(colnames_fn == .x)], .cols = colnames_fn) %>% # rename footnotes columns to respective years
      pivot_longer(
        all_of(colnames_y),
        names_to = "year",
        values_to = "rep"
      )
  )


# combine tibbles for all year groups into one
ifelse(
  nrow(all_data) == 0,
  all_data <- bgs_data,
  all_data <- all_data %>% union(., bgs_data)
)



}




# formatting columns
all_data <- all_data %>%
  mutate(
    year = as.integer(year),
    value = as.double(value)
  ) %>%
  arrange(commodity, country, sub_commodity, year)

all_data <- all_data %>%
  mutate(
    sub_commodity = ifelse(sub_commodity == "", NA, sub_commodity),
    rep = ifelse(rep == "", NA, rep)
  )



# save rds
year_min <- min(all_data$year) %>% as.character
year_max <- max(all_data$year) %>% as.character

write_rds(all_data, paste0("./03_intermediate/02_data_harmonization/clean_bgs",year_min,"_",year_max,".rds"))



