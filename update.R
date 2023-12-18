



## loading packages

# check if the necessary packages are installed and if not, install them, then load them
req_packages <- c("tidyverse","plyr","readxl","jsonlite","httr","RCurl","XML", "data.table")
req_packages <- req_packages[!req_packages %in% installed.packages()]

lapply(req_packages, install.packages)





## load functions
source("./02_scripts/00_functions/geographic_adjust.R")

source("./02_scripts/00_functions/lastfile.R")






#### Cleaning / Formatting / Harmonization

### Metals and non-metallic minerals (WMD, BGS, USGS)


## WMD

# download data
# source("./02_scripts/01_data_retrieval/retrieve_wmd.R")#works

# clean data
source("./02_scripts/02_data_harmonization/clean_wmd.R") 

# harmonize
source("./02_scripts/02_data_harmonization/harmonize_wmd.R")



## BGS

# download data
# source("./02_scripts/01_data_retrieval/retrieve_bgs.R")

# clean data
  # (in case of warning for coercing NAs -> is okay)
source("./02_scripts/02_data_harmonization/clean_bgs.R")

# harmonize
source("./02_scripts/02_data_harmonization/harmonize_bgs.R")



## USGS

# download data
# source("./02_scripts/01_data_retrieval/retrieve_usgs.R")

# clean data
# source("./02_scripts/02_data_harmonization/clean_usgs.R")

# harmonize
source("./02_scripts/02_data_harmonization/harmonize_usgs.R")






### Fossil fuels (UNSD, IEA, EIA)

## UNSD

# data download: Not necessary, because files provided by UN
  # files saved in ./03_intermediate/01_data_retrieval/unsd

# clean data: Not necessary, as data already in proper format

# harmonize
source("./02_scripts/02_data_harmonization/harmonize_unsd.R")



## IEA

# data download: manually from OECD iLibrary
  # files saved in ./03_intermediate/01_data_retrieval/iea

# clean data: Not necessary, as data already in proper format

# harmonize
source("./02_scripts/02_data_harmonization/harmonize_iea.R")



## EIA

# download data
# source("./02_scripts/01_data_retrieval/retrieve_eia.R")

# clean data
source("./02_scripts/02_data_harmonization/clean_eia.R")

# harmonize
source("./02_scripts/02_data_harmonization/harmonize_eia.R")







#### Conversion 1 (oxides to elemental metals/minerals)

# element conversion
source("./02_scripts/04a_data_conversion/conversion_elements.R")







#### Integration

# WMD-BGS-USGS-WMD_ext (metals and non-metallic minerals)
source("./02_scripts/03_data_integration/integration_wmd_bgs_usgs.R")

# UNSD-IEA-EIA (fossil fuels)
source("./02_scripts/03_data_integration/integration_fossil_fuels.R")

# Integrate the above
source("./02_scripts/03_data_integration/integration_all.R")


#### USGS Extension and re integration

# create table USGS extension
source("./02_scripts/01_data_retrieval/usgs_manual_extension.R")

# clean and harmonize USGS extension 
source("./02_scripts/02_data_harmonization/clean_harmonize_usgs_extensions.R")

# element conversion with usgs extension
source("./02_scripts/04a_data_conversion/conversion_elements_usgs_ext.R")

#combine USGS extension output from 2021 with current output
source("./02_scripts/02_data_harmonization/combine_usgs_extensions.R")


#integrate again
#make sure that in th extension file Simon´s work is integrated and the missing data is  filled in
#source(...)
source("./02_scripts/03_data_integration/integration_wmd_bgs_usgs.R")
source("./02_scripts/03_data_integration/integration_all.R")







#### Conversion 2 (units)

# unit conversion
source("./02_scripts/04a_data_conversion/conversion_units.R")







#### Estimations

# estimation of ores
source("./02_scripts/04b_data_estimation/estimation_ores_smooth.R")

#til here executed 02.11.2022 ###########################################################################################################

# estimation and integration for construction-dominant minerals
source("./02_scripts/04b_data_estimation/estimation_construction_min.R")







#### Final cleaning/formatting

# final cleaning
source("./02_scripts/06_final_formatting/final_cleaning.R")

# adjustment of outliers (with respective html output showing outliers)
wd <- getwd()

rmarkdown::render("./02_scripts/05_data_check/check_large_changes.Rmd",
                  knit_root_dir = wd,
                  output_dir = "./04_output/07_checks/",
                  intermediates_dir = "./04_output/07_checks/",
                  output_file = paste0("check_large_changes_", substr(Sys.time(), 1, 10),".html")
                  )

# gap filling and saving of final data files (CCC and detailed)
source("./02_scripts/06_final_formatting/final_formatting.R")







#### Final Checks

# final checks
source("./02_scripts/05_data_check/final_checks.R")


# comparison to various former compilation versions (with html output)
wd <- getwd()
rmarkdown::render("./02_scripts/05_data_check/01_other/ccc_comparison_now_former.Rmd",
                  knit_root_dir = wd,
                  output_dir = "./04_output/07_checks/",
                  intermediates_dir = "./04_output/07_checks/",
                  output_file = paste0("ccc_comparison_now_former_", substr(Sys.time(), 1, 10),".html")
)







