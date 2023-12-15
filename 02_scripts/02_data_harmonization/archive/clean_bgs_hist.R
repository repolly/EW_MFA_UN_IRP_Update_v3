

# #### clean historic bgs data -------------------------------------------------------
# 
# bgs_hist <- read_delim("./01_input/04_former_data/bgs/bgs_data_2016-12-12.csv", delim = ",")
# 
# 
# # remove extra ID column and NAs
# bgs_hist <- bgs_hist[,-1]
# #bgs_hist[is.na(bgs_hist)] <- c("")
# 
# 
# 
# # define column numbers for everything not being footnotes
# # (first four and then every second one)
# colnr <- vector()
# length(colnr) <- (ncol(bgs_hist)+4)/2
# colnr[1:4] <- c(1:4) # no data in these columns
# bgs_hist[,90:94]
# for (i in 5:length(colnr)) {
#   colnr[i] <- (2*i-4)
# }
# 
# # define column numbers for footnote columns
# # only taking into account everything starting from column 5
# colnr_fn <- colnr[5:length(colnr)] - 1
# 
# 
# # column names for years
# years <- 1970:2014
# 
# # provide column names to table (for everything except footnotes)
# colnames(bgs_hist)[colnr] <- c("commodity", "unit", "country", "sub_commodity", years)
# 
# # define available years (again in a flexible way, in case code should be adjusted for different year ranges in the future)
# colnames_y <- as.numeric(colnames(bgs_hist))
# colnames_y <- (min(colnames_y, na.rm = TRUE):max(colnames_y, na.rm = TRUE))
# colnames_y <- as.character(colnames_y)
# 
# # create name templates 
# colnames_fn <- paste0("fn_", colnames_y)
# 
# # provide names to footnote columns
# colnames(bgs_hist)[colnr_fn] <- colnames_fn
# 
# 
# 
# # turn data into long format
# # first exclude footnote columns, then switch years/values to long format,
# # then left join with table where footnotes have been switched to long format
# bgs_hist <- bgs_hist %>%
#   select(-all_of(colnames_fn)) %>%
#   pivot_longer(
#     all_of(colnames_y),
#     names_to = "year",
#     values_to = "value"
#   ) %>%
#   left_join(
#     bgs_hist %>%
#       select(-all_of(colnames_y)) %>%
#       rename_with(~ colnames_y[which(colnames_fn == .x)], .cols = colnames_fn) %>% # rename footnotes columns to respective years
#       pivot_longer(
#         all_of(colnames_y),
#         names_to = "year",
#         values_to = "rep"
#       )
#   )
# 
# 
# # formatting columns
# bgs_hist <- bgs_hist %>%
#   mutate(
#     year = as.integer(year),
#     value = as.double(value)
#   )
# 
# bgs_hist <- bgs_hist %>%
#   mutate(
#     sub_commodity = ifelse(sub_commodity == "", NA, sub_commodity),
#     rep = ifelse(rep == "", NA, rep)
#   )
# 
# 
# 
# 
# 
# 
# ### Combine recent and historic data
# 
# ## remove years from historic data (which are double with recent data)
# 
# # minimum year of recent data
# years <- min(bgs_rec$year)
# 
# # remove obsolete years
# bgs_hist <- bgs_hist %>% filter(year < years)
# 
# 
# # combine both tibbles
# bgs <- bgs_rec %>%
#   union(., bgs_hist)
# 
# a1 <- bind_rows(bgs_rec, bgs_hist)
# 
# 
# # remove large obsolete tibbles from R environment
# rm(bgs_rec, bgs_hist)
# 
# 
# 
# 
# 
# 
# 
# 
# # save rds
# year_min <- min(bgs_hist$year) %>% as.character
# year_max <- max(bgs_hist$year) %>% as.character
# 
# write_rds(bgs_hist, paste0("./03_intermediate/02_data_harmonization/clean_bgs_hist_",year_min,"_",year_max,".rds"))

