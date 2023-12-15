##### Integration of all metals and non-metallic minerals (From WMD, BGS, USGS)
  ### Steps:
    # Create pre-defined lists
      # ores/minerals
      # mat_ids and associated_mat_ids
      # associated_mat_ids of all mat_ids (not minerals)
    # Remove obsolete time series from all datasets (i.e. with only values = 0)
    # Save table with all data combined and aggregate everything for countries which merged across time
      # in order to do integration with aggregated table in order to avoid double counting and re-combine with disaggregated data at end
      # Aggregate all in loop based on cou_for_aft
    # WMD / BGS
      # Save all BGS data before 1984 (because that's when WMD data starts)
      # Select source based on certainty of value (i.e. footnote: reported or estimated)
      # Get intermediate selection from bgs where there are more reported values for a material per country than in wmd data
      # Filter for everything else from WMD
      # Filter for everything else from BGS
      # Filter out everything from BGS which would be associated with a material from WMD (not including minerals)
      # Combine intermediate tables + bgs_1983
      # Create list of all ores/minerals which would also be covered by associated materials in intermediate data
      # Select minerals (i.e. everything which already represents extraction and therefore has priority over values representing content)
      # Get associated mat_ids for minerals above (i.e. materials already covered by their respective minerals)
      # Extend associated mat_ids for elements of agg_mat_ids (i.e. materials in aggregated categories)
      # Extend associated mat_ids for anything related to associated IDs above and not being a mineral/ore (e.g. Nm.PO being related to Nm.P)
      # Filter out associated materials from intermediate data, because DE already covered by ores/minerals
      # Filter out agg_mat_ids for countries where material in agg_mat_id is already included (int_data_3)
    # USGS
      # Do the same filtering as above, but also combine with respective associated IDs from WMD-BGS
        # Select minerals (i.e. everything which already represents extraction and therefore has priority over values representing content)
        # Get associated mat_ids for minerals above (i.e. materials already covered by their respective minerals)
        # Extend associated mat_ids for elements of agg_mat_ids (i.e. materials in aggregated categories)
        # Extend associated mat_ids for anything related to associated IDs above and not being a mineral/ore (e.g. Nm.PO being related to Nm.P)
        # Extend associated IDs with those from WMD-BGS
        # Filter out associated materials from intermediate data, because DE already covered by ores/minerals
        # Filter out anything related to associated IDs already included in data (not being a mineral)
        # Extend associated IDs with those from WMD-BGS
        # Filter out agg_mat_ids for countries where material in agg_mat_id is already included (int_data_3)
          # Extend by those from WMD-BGS
        # Filter out everything from usgs which would be associated with a material from wmd_bgs (not including minerals)
    # Combine everything
    # Filter out stuff temporarily which is likely to cause double counting
    # Filter out "other products"
    # Add disaggregated data again
      # Remove resulting NAs from re-adding disaggregated data







library(tidyverse)


# clear R environment
rm(list = ls())

## read files

# data

wmd <- read_rds("./03_intermediate/04a_data_conversion/conversion_elements/wmd_conversion_1.rds") %>%
  mutate(source = "wmd")

unique(wmd$year)

bgs <- read_rds("./03_intermediate/04a_data_conversion/conversion_elements/bgs_conversion_1.rds") %>%
  mutate(source = "bgs")

unique(bgs$year)

usgs <- read_rds("./03_intermediate/04a_data_conversion/conversion_elements/usgs_conversion_1.rds") %>%
  mutate(source = "usgs")
unique(usgs$year)
usgs[usgs$year==2019,]
wmd_rep <- read_rds("./03_intermediate/04a_data_conversion/conversion_elements/wmd_rep_conversion_1.rds") %>%
  mutate(source = "wmd_rep")


# IDs
mat_ids <- read_delim("./01_input/01_concordance_tables/material_ids.csv", delim = ";")

# special cases
double_counting <- read_delim("./01_input/02_log_files/double_counting.csv", ";")


# lists
cou_for_aft <- read_delim("./01_input/01_concordance_tables/country_former_after.csv", delim = ";")









## pre-defined lists

# ores/minerals
list_min <- mat_ids %>% 
  filter(material_category == "mineral") %>%
  select(material_id) %>%
  pull()

# list of mat_ids and associated_mat_ids
ass_ids <- mat_ids %>%
  distinct(material_id, associated_mat_id) %>%
  filter(!is.na(material_id)) %>%
  filter(!is.na(associated_mat_id)) %>%
  rename(
    min_id = material_id,
    material_id = associated_mat_id
  )

# list of associated_mat_ids of all mat_ids (not minerals)
ass_ids_2 <- mat_ids %>%
  filter(material_category != "mineral") %>%
  distinct(material_id, associated_mat_id) %>%
  filter(!is.na(material_id)) %>%
  filter(!is.na(associated_mat_id)) %>%
  left_join(.,
            mat_ids %>%
              filter(material_category != "mineral") %>%
              distinct(material_id, associated_mat_id) %>%
              filter(!is.na(material_id)) %>%
              filter(!is.na(associated_mat_id)) %>%
              rename(associated_mat_id_2 = material_id)
            ) %>%
  filter(material_id != associated_mat_id_2)

ass_ids_2 <- ass_ids_2 %>%
  union(.,
        ass_ids_2 %>%
          distinct(material_id, associated_mat_id) %>%
          mutate(associated_mat_id_2 = associated_mat_id)
        ) %>%
  union(.,
        ass_ids_2 %>%
          distinct(material_id, associated_mat_id) %>%
          mutate(associated_mat_id_2 = material_id) %>%
          mutate(material_id = associated_mat_id)
        ) %>%
  select(-associated_mat_id)




## Remove obsolete time series from all datasets (i.e. with only values = 0)
for (i in c("wmd", "bgs", "usgs", "wmd_rep")) {
  
  # i="wmd"#2016 to 2020
  # i="bgs"#2016 to 2020
  # i="usgs"#2016 to 2021
  # i="wmd_rep"#2015 to 2017
  
  
  df <- eval(parse(text = i)) %>%
      group_by(alphanumiso, material_id) %>%
      summarise(value = sum(value, na.rm = TRUE)) %>%
      ungroup() %>%
      filter(value != 0) %>%
      select(-value) %>%
      left_join(., eval(parse(text = i)))
  
  unique(df$year)
  
  if(i == "wmd") wmd <- df
  if(i == "bgs") bgs <- df
  if(i == "usgs") usgs <- df
  if(i == "wmd_rep") wmd_rep <- df
  
}



## save table with all data combined and aggregate everything for countries which merged across time
  ## do integration with aggregated table in order to avoid double counting and re-combine with disaggregated data at end
prior_data <- wmd %>%
  select(-rep) %>%
  bind_rows(., bgs %>% select(-rep)) %>%
  bind_rows(., usgs)


# aggregate all in loop based on cou_for_aft
for (i in c("wmd", "bgs", "usgs")) {
  
  col_names <- names(eval(parse(text = i)))[names(eval(parse(text = i))) != "value"]
  
  df <- eval(parse(text = i)) %>%
    left_join(.,
              cou_for_aft %>%
                filter(category == "merge") %>%
                select(-category) %>%
                rename(alphanumiso = former)
              ) %>%
    mutate(
      alphanumiso = ifelse(
        !is.na(after),
        after,
        alphanumiso
        )
      ) %>%
    group_by(across(all_of(col_names))) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup()
  
  if(i == "wmd") wmd <- df
  if(i == "bgs") bgs <- df
  if(i == "usgs") usgs <- df
  if(i == "wmd_rep") wmd_rep <- df
  
}





### WMD / BGS ----


## save all BGS data before 1984 (because that's when WMD data starts)
bgs_1983 <- bgs %>%
  filter(year <= 1983) %>%
  select(-rep)

# remove respective years from bgs
bgs <- bgs %>%
  filter(year >= 1984)



# union WMD and BGS (BGS only 1984 onwards)
wmd_bgs <- wmd %>%
  union(., bgs)




## select source based on certainty of value (i.e. footnote: reported or estimated)
  # only for years 2015-2017, because info from WMD only available for these years (but selection is used for whole time series, nonetheless)
wmd_rep_bgs <- wmd_rep %>%
  union(.,
        bgs %>%
          filter(year %in% c(2015:2017)) %>%
          select(-unit_id)
        ) %>%
  select(-value)


# get intermediate selection from bgs where there are more reported values for a material per country than in wmd data
  # only necessary to compare "r" values, because these are the only ones where BGS is chosen over WMD if higher, otherwise it's always WMD
int_sel_1 <- wmd_rep_bgs %>%
  filter(rep == "r") %>%
  group_by(alphanumiso, material_id, source, rep) %>% 
  summarize(count = n()) %>%
  ungroup() %>%
  pivot_wider(.,
              names_from = source,
              values_from = count
              ) %>%
  mutate(
    final_source = ifelse(
      bgs > wmd_rep | (is.na(wmd_rep) & !is.na(bgs)),
      "bgs",
      "wmd"
    )
  ) %>%
  filter(final_source == "bgs") %>%
  select(alphanumiso, material_id)



# filter for everything else from WMD
  # because int_sel_1 simply specifies what to not take from WMD for same materials
int_data_1_wmd <- setdiff(
  wmd %>%
    distinct(alphanumiso, material_id),
  int_sel_1
  ) %>%
  left_join(., wmd) %>%
  select(-rep)


# filter for everything else from BGS
int_data_2_bgs <- setdiff(
  bgs %>%
    distinct(alphanumiso, material_id),
  int_data_1_wmd %>%
    distinct(alphanumiso, material_id)
) %>%
  left_join(., bgs) %>%
  select(-rep)



# filter out everything from BGS which would be associated with a material from WMD (not including minerals)
wmd_ass_cov <- int_data_1_wmd %>%
  left_join(.,
            ass_ids_2
  ) %>%
  filter(!is.na(associated_mat_id_2)) %>%
  distinct(alphanumiso, associated_mat_id_2, year) %>%
  rename(material_id = associated_mat_id_2)

int_data_2_bgs <- setdiff(
  int_data_2_bgs %>%
    select(alphanumiso, year, material_id),
  wmd_ass_cov
  ) %>%
  left_join(., int_data_2_bgs)



# combine intermediate tables + bgs_1983
int_data_3 <- int_data_1_wmd %>%
  union(., int_data_2_bgs) %>%
  union(., bgs_1983) 







# list of all ores/minerals which would also be covered by associated materials in intermediate data
mat_ass_cov <- int_data_3 %>%
  filter(value > 0) %>%
  left_join(.,
            ass_ids) %>%
  distinct(alphanumiso, year, min_id) %>%
  filter(!is.na(min_id)) %>%
  rename(material_id = min_id) %>%
  filter(material_id %in% list_min)





# select minerals (i.e. everything which already represents extraction and therefore has priority over values representing content)
part_1_minerals <- int_data_3 %>%
  filter(material_id %in% (mat_ids %>% 
                             filter(material_category == "mineral") %>% 
                             select(material_id) %>% 
                             pull()
  )
  )



# get associated mat_ids for minerals above (i.e. materials already covered by their respective minerals)
min_ass_cov <- part_1_minerals %>%
  left_join(.,
            mat_ids %>%
              filter(material_category == "mineral") %>%
              select(material_id, associated_mat_id)
            ) %>%
  filter(!is.na(associated_mat_id)) %>%
  distinct(alphanumiso, year, associated_mat_id)



# extend associated mat_ids for elements of agg_mat_ids (i.e. materials in aggregated categories)
min_ass_cov <- min_ass_cov %>%
  union(.,
        min_ass_cov %>%
          left_join(.,
                    mat_ids %>%
                      select(material_id_agg, material_id),
                    by = c("associated_mat_id" = "material_id_agg")
          ) %>%
          filter(!is.na(material_id)) %>%
          select(-associated_mat_id) %>%
          rename(associated_mat_id = material_id)
        )


# extend associated mat_ids for anything related to associated IDs above and not being a mineral/ore (e.g. Nm.PO being related to Nm.P)
min_ass_cov <- min_ass_cov %>%
  union(.,
        min_ass_cov %>%
          left_join(.,
                    mat_ids %>%
                      filter(material_category != "mineral") %>%
                      select(material_id, associated_mat_id),
                    by = c("associated_mat_id")
          ) %>%
          filter(!is.na(material_id)) %>%
          select(-associated_mat_id) %>%
          rename(associated_mat_id = material_id)
  )


# filter out associated materials from intermediate data, because DE already covered by ores/minerals)
int_data_3 <- setdiff(
  int_data_3 %>%
    distinct(alphanumiso, material_id, year),
  min_ass_cov %>%
    rename(material_id = associated_mat_id)
  ) %>%
  left_join(., int_data_3)







# filter out agg_mat_ids for countries where material in agg_mat_id is already included (int_data_3)
agg_cov_1 <- int_data_3 %>%
  left_join(., 
            mat_ids %>%
              distinct(material_id, material_id_agg) %>%
              filter(!is.na(material_id_agg)) %>%
              filter(!is.na(material_id))
            ) %>%
  filter(!is.na(material_id_agg)) %>%
  distinct(alphanumiso, material_id_agg, year) %>%
  rename(material_id = material_id_agg)


int_data_3 <- setdiff(
  int_data_3 %>%
    distinct(alphanumiso, material_id, year),
  agg_cov_1
  ) %>%
  left_join(., int_data_3)











### USGS

## do the same filtering as above, but also combine with respective associated IDs from WMD-BGS

# select minerals (i.e. everything which already represents extraction and therefore has priority over values representing content)
part_2_minerals <- usgs %>%
  filter(material_id %in% (mat_ids %>% 
                             filter(material_category == "mineral") %>% 
                             select(material_id) %>% 
                             pull()
  )
  )



# get associated mat_ids for minerals above (i.e. materials already covered by their respective minerals)
min_ass_cov_2 <- part_2_minerals %>%
  left_join(.,
            mat_ids %>%
              filter(material_category == "mineral") %>%
              select(material_id, associated_mat_id)
  ) %>%
  filter(!is.na(associated_mat_id)) %>%
  distinct(alphanumiso, year, associated_mat_id)



# extend associated mat_ids for elements of agg_mat_ids (i.e. materials in aggregated categories)
min_ass_cov_2 <- min_ass_cov_2 %>%
  union(.,
        min_ass_cov_2 %>%
          left_join(.,
                    mat_ids %>%
                      select(material_id_agg, material_id),
                    by = c("associated_mat_id" = "material_id_agg")
          ) %>%
          filter(!is.na(material_id)) %>%
          select(-associated_mat_id) %>%
          rename(associated_mat_id = material_id)
  )


# extend associated mat_ids for anything related to associated IDs above and not being a mineral/ore (e.g. Nm.PO being related to Nm.P)
min_ass_cov_2 <- min_ass_cov_2 %>%
  union(.,
        min_ass_cov_2 %>%
          left_join(.,
                    mat_ids %>%
                      filter(material_category != "mineral") %>%
                      select(material_id, associated_mat_id),
                    by = c("associated_mat_id")
          ) %>%
          filter(!is.na(material_id)) %>%
          select(-associated_mat_id) %>%
          rename(associated_mat_id = material_id)
  )


# extend associated IDs with those from WMD-BGS
min_ass_cov_2 <- min_ass_cov_2 %>%
  union(., min_ass_cov)



# filter out associated materials from intermediate data, because DE already covered by ores/minerals)
usgs <- setdiff(
  usgs %>%
    distinct(alphanumiso, material_id, year),
  min_ass_cov %>%
    rename(material_id = associated_mat_id)
) %>%
  left_join(., usgs)


usgs[usgs$year==2019,]

## filter out anything related to associated IDs already included in data (not being a mineral)
mat_ass_cov_3 <- usgs %>%
  left_join(.,
            mat_ids %>%
              filter(material_category != "mineral") %>%
              select(material_id, associated_mat_id),
            by = c("material_id" = "associated_mat_id"),
            suffix = c("", "_ass")
  ) %>%
  filter(!is.na(material_id_ass)) %>%
  select(-material_id) %>%
  rename(material_id = material_id_ass) %>%
  distinct(alphanumiso, year, material_id)


# extend associated IDs with those from WMD-BGS
  # and by all associated IDs from WMD-BGS including minerals/ores
mat_ass_cov_3 <- mat_ass_cov_3 %>%
  #union(., mat_ass_cov_2) %>%
  union(.,
        int_data_3 %>%
          left_join(.,
                    mat_ids %>%
                      distinct(material_id, associated_mat_id),
                    by = c("material_id" = "associated_mat_id"),
                    suffix = c("", "_ass")
          ) %>%
          filter(!is.na(material_id_ass)) %>%
          select(-material_id) %>%
          rename(material_id = material_id_ass) %>%
          distinct(alphanumiso, year, material_id)
        )

mat_ass_cov_4 <- int_data_3 %>%
  left_join(.,
            ass_ids,
            by = c("material_id" = "min_id"),
            suffix = c("", "_ass")
            ) %>%
  filter(!is.na(material_id_ass)) %>%
  select(alphanumiso, year, material_id_ass) %>%
  rename(material_id = material_id_ass)

mat_ass_cov_4 <- mat_ass_cov_4 %>%
  union(.,
        mat_ass_cov_4 %>%
          left_join(., ass_ids) %>%
          select(alphanumiso, year, min_id) %>%
          rename(material_id = min_id)
        )





# filter
usgs <- setdiff(
  usgs %>%
    distinct(alphanumiso, material_id, year),
  mat_ass_cov_3 %>%
    union(mat_ass_cov_4)
) %>%
  left_join(., usgs)

usgs[usgs$year==2019,]


## filter out agg_mat_ids for countries where material in agg_mat_id is already included (int_data_3)
agg_cov_2 <- usgs %>%
  left_join(., 
            mat_ids %>%
              distinct(material_id, material_id_agg) %>%
              filter(!is.na(material_id_agg)) %>%
              filter(!is.na(material_id))
  ) %>%
  filter(!is.na(material_id_agg)) %>%
  distinct(alphanumiso, material_id_agg, year) %>%
  rename(material_id = material_id_agg)


# extend by those from WMD-BGS
agg_cov_2 <- agg_cov_2 %>%
  union(., agg_cov_1)


# filter
usgs <- setdiff(
  usgs %>%
    distinct(alphanumiso, material_id, year),
  agg_cov_2
) %>%
  left_join(., usgs)

usgs[usgs$year==2019,]


# filter out everything from usgs which would be associated with a material from wmd_bgs (not including minerals)
wmd_bgs_ass_cov <- int_data_3 %>%
  left_join(.,
            ass_ids_2
  ) %>%
  filter(!is.na(associated_mat_id_2)) %>%
  distinct(alphanumiso, associated_mat_id_2, year) %>%
  rename(material_id = associated_mat_id_2)

usgs <- setdiff(
  usgs %>%
    select(alphanumiso, year, material_id),
  wmd_bgs_ass_cov
) %>%
  left_join(., usgs)

usgs[usgs$year==2019,]






### combine everything

# filter out from usgs what is already included in int_data_3
usgs <- setdiff(
  usgs %>%
    distinct(alphanumiso, material_id, year),
  int_data_3 %>%
    distinct(alphanumiso, material_id, year)
  ) %>%
  left_join(., usgs)

usgs[usgs$year==2019,]
int_data_3[int_data_3$alphanumiso=="GEO268",]

# combine
all_data <- int_data_3 %>%
  union(., usgs)





# filter out stuff temporarily which is likely to cause double counting
all_data <- setdiff(
  all_data %>%
    distinct(source, material_id),
  double_counting
  ) %>%
  left_join(., all_data)




# filter out "other products"
all_data <- all_data %>%
  left_join(.,
            mat_ids %>%
              select(material_id, material_category_2)
            ) %>%
  filter(!material_category_2 %in% c("metal alloy", "metal product", "metal ore product")) %>%
  select(-material_category_2)






## add disaggregated data again
  # taking into account to only include the geographic entity selected by integration and being available in initial data
all_data <- all_data %>%
  filter(
    alphanumiso %in% (cou_for_aft %>%
                        filter(category == "merge") %>%
                        distinct(after) %>%
                        pull()
                      )
    ) %>%
  left_join(.,
            cou_for_aft %>%
              filter(category == "merge") %>%
              select(-category) %>%
              rename(alphanumiso = after)
  ) %>%
  select(former, year, material_id, source) %>%
  rename(alphanumiso = former) %>%
  union(.,
        all_data %>%
          select(alphanumiso, year, material_id, source)
        ) %>%
  left_join(., prior_data)


# remove resulting NAs from re-adding disaggregated data
all_data <- all_data %>%
  filter(!is.na(value))
  



## remove data which should not be included

# aluminium
all_data <- all_data %>%
  filter(!material_id %in% c("Me.Al", "Me.Al2O3"))





# overview coverage by source
all_data %>%
  group_by(source) %>%
  summarise(n = n())




all_data <- all_data[all_data$year>=2016,]
# save rds
write_rds(all_data, file = "./03_intermediate/03_data_integration/wmd_bgs_usgs_integrated.rds")
unique(all_data$year)





