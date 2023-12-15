##### Retrieve Excel file from WMD website

#url <- "https://www.world-mining-data.info/wmd/downloads/XLS/6.4.Production_of_Mineral_Raw_Materials_of_individual_Countries_by_Minerals.xlsx"
url <- "https://www.world-mining-data.info/wmd/downloads/XLS/6.4.%20Production_of_Mineral_Raw_Materials_of_individual_Countries_by_Minerals.xlsx"
download.file(
  url,
  destfile = paste0(
    "./03_intermediate/01_data_retrieval/wmd/wmd_raw_",
    substr(Sys.time(),1,10),
    ".xlsx"
    ), 
  mode="wb"
  )

