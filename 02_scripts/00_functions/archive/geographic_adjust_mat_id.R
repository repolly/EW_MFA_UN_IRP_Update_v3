### Adjustment of values according to actual geographic existence of countries

library(tidyverse)

# datainput <- bgs

# Table with country groups and years of existence from csv-file
geo_exist <- readr::read_delim("./01_input/01_concordance_tables/geo_exist.csv", ";")

#change integer to numeric for easier use in further processes
geo_exist <- geo_exist %>% mutate_if(is.integer, as.numeric)


# Function
geographic_adjust_mat_id <- function(datainput){

  # List of former country groups (just randomly selected as those having a last year)
  former_list <- geo_exist %>% filter(!is.na(last_year)) %>% pull(alphanumiso)
  
  # First loop for each country group
  for (i in former_list) {
    
  #  i <- former_list[3]
    
    # Setting first and last year for each country group (1980 as default first year if no other)
    # Also setting the countries belonging to the selected country group
    cg_row <- geo_exist %>% filter(alphanumiso == i)
    firstyear <- if_else(is.na(cg_row$first_year), 1980, cg_row$first_year)
    lastyear <- cg_row$last_year
    countries <- geo_exist %>% filter(former == i) %>% pull(alphanumiso)

    # Second loop for each year
    # Years are not included in "group by", but done step by step in a loop,
    ## because it seems that otherwise this would mess with actually reported values for the 
    ## country groups. So an if-clause for each year seems necessary. 
    for (j in firstyear:lastyear) {
      
     # j <- 1991
      
      for (k in unique(datainput$material_id)) {
        
     # k <- unique(datainput$material_id)
     #   k <- unique(datainput$material_id)[grep("natural gas",k)]
        
      
      # Checking if there are even values larger zero for member countries when there shouldn't.
      # And if value for country group is actually 0 or NA (not really necessary, but better
      # to just avoid any risk of double counting).
      if (sum(datainput[datainput$alphanumiso %in% countries & datainput$year == j & datainput$material_id == k,]$value, na.rm = TRUE) > 0  &
         (is.na(datainput[datainput$alphanumiso == i & datainput$year == j & datainput$material_id == k,]$value) ||  
         (!(i %in% unique(datainput[datainput$year == j & datainput$material_id == k,]$alphanumiso)))            
         )
         )
        {
  
        # Aggregate the values for the country group's member states
        agg_values <- datainput %>%
          filter(alphanumiso %in% countries & year == j & material_id == k) %>%
          group_by(year, material_id, unit) %>% 
          summarise(new_value = sum(value, na.rm = TRUE)) %>%
          ungroup() %>% 
          mutate("alphanumiso" = i, "material_id" = k)
          
        
        
        # Join the aggregated values to the data table and include the relevant values
        datainput <- datainput %>%
          full_join(., agg_values, by = c("material_id", "year", "alphanumiso")) %>% 
          mutate(value = ifelse(!(is.na(new_value) | new_value == 0) , new_value, value)) %>%  # correct the value for the former country
          mutate(value = ifelse(alphanumiso %in% countries & year == j, NA, value)) %>% # set the old values NA to avoid double counting
          mutate(unit = ifelse(!is.na(unit.y), unit.y, unit.x)) %>% 
          select(-c(new_value, unit.y, unit.x)) # deselect the old value
        
        rm(agg_values)

        # datainput2 <- datainput
         #datainput %>% filter(year %in% 2002:2005, material_id == "Production of platinum group metals, mine", alphanumiso == "SRB688")
         #datainput %>% filter(year %in% 2002:2005, material_id == "Production of platinum group metals, mine", alphanumiso == "SCG891")
        
        }        
      }
    }
  }
  
  return(datainput)
  
}

# 
# bgs2 <- geographic_adjust(bgs)
# 
# 
# 
# datainput %>%
#   filter(former == "CSK200") %>%
#   group_by(year) %>%
#   summarise(value = sum(value, na.rm = TRUE))
# 
# datainput %>%
#   filter(alphanumiso == "CSK200") %>%
#   group_by(year) %>%
#   summarise(value = sum(value, na.rm = TRUE))
# 
# bgs2 %>%
#   filter(alphanumiso == "CSK200") %>%
#   group_by(year) %>%
#   summarise(value = sum(value, na.rm = TRUE))
# 
# bgs2 %>%
#   filter(former == "CSK200") %>%
#   group_by(year) %>%
#   summarise(value = sum(value, na.rm = TRUE))

