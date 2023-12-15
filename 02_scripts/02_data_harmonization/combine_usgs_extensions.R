##### Combine outputs from  2021 & 2022 and keep the latest ones
##### both files are already converted, so no formatting needed





library(tidyverse)
library(readxl)

# clear R environment
rm(list = ls())




## read files

# extension data sets and there respective input files
usgs_ext_1 <- read_rds("./03_intermediate/04a_data_conversion/conversion_elements/usgs_conversion_2021.rds")%>%
  mutate(source = "usgs")
usgs_ext_2 <- read_rds("./03_intermediate/04a_data_conversion/conversion_elements/usgs_conversion_1.rds")%>%
  mutate(source = "usgs")

for (i in 1:nrow(usgs_ext_2))
{
  a <- usgs_ext_2$alphanumiso[i]
  m <- usgs_ext_2$material_id[i]
  u <- usgs_ext_2$unit_id[i]
  y <- usgs_ext_2$year[i]
  v <- usgs_ext_2$value[i]
  if (is.na(v)==F)
  {
    for (j in nrow(usgs_ext_1))
    {
      a1 <- usgs_ext_1$alphanumiso[j]
      m1 <- usgs_ext_1$material_id[j]
      u1 <- usgs_ext_1$unit_id[j]
      y1 <- usgs_ext_1$year[j]
      v1 <- usgs_ext_1$value[j]
      if (is.na(v1)==F)
      {
        if(a1==a && y1==y && m1==m && u1==u && v1!=v)
        {
          usgs_ext_1$value[j] <- usgs_ext_2$value[i]
          print(paste0("changes for: ",a," && ",m," && ",u," && ",y," && ",v," && ",v1))
        }
      }
    }
  }
}

usgs_ext_1 <- usgs_ext_1[usgs_ext_1$year>2015,]
# save rds
write_rds(usgs_ext_1, file = "./03_intermediate/04a_data_conversion/conversion_elements/usgs_conversion_1.rds")

