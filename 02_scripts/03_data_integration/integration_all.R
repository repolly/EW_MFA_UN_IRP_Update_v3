##### Integration of metals & non-metallic minerals (wmd, bgs, usgs) with fossil fuels (unsd, iea, eia)




library(tidyverse)


# clear R environment
rm(list = ls())




## read files

# data

minerals <- read_rds("./03_intermediate/03_data_integration/wmd_bgs_usgs_integrated.rds")

fossils <- read_rds("./03_intermediate/03_data_integration/fossils_integrated.rds")




data_integrated <- minerals %>%
  union(., fossils)




# save rds
write_rds(data_integrated, file = "./03_intermediate/03_data_integration/all_data_integrated.rds")


