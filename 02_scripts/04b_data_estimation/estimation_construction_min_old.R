##### Estimation of construction minerals
  ### Steps:
    # Load all necessary input data
    # Harmonize UNSD cement production
    # Harmonize USGS cement
    # Combine cement data
    # Cement trade harmonization
    # Sand trade harmonization
    # Gravel trade harmonization
    # Brick production harmonization
    # Clay tile production harmonization
    # Bitumen harmonization
    # Calculations
      # Cement estimations
        # Reduce to maximum year of production
      # Bricks estimations
      # Clay tiles estimations
      # Bitumen estimations
    # Integrate/combine data
      # Ensure same time coverage for data points which are combined (i.e. M.coc and M.sag)
      # M.coc (cement, bricks, tiles)
        # Extend M.coc time series for cement estimations
        # Extend M.coc time series for bricks estimations
        # Extend M.coc time series for tiles estimations
      # M.sag (cement, bitumen)
        # Extend M.sag time series for cement estimations
        # Extend M.sag time series for bitumen estimations
      # Combine current estimations and former estimations
        # Extend former estimations for specific countries
    # Correct estimations for sand and gravel with respective trade data
    # Integrate with reported data
      # Aggregate data for M.sag and M.crs
      # Integrate
      # Add detail for M.sag and M.crs again







library(tidyverse)
library(readxl)


# clear R environment
rm(list = ls())





## read files

# intermediate data (for integration at the end)
data <- read_rds("./03_intermediate/4b_data_estimation/data_estimated_ores.rds")


# other data (for estimation)
cement_prod <- read_delim("./03_intermediate/01_data_retrieval/constr_min/unsd_cement_production.txt", delim = ";")
# names(cement_prod)[5] <- "Value Footnotes"
usgs_cem <- read_delim("./03_intermediate/01_data_retrieval/usgs/cement/cement_2012-2018.csv", ";")

cement_trade <- read_delim("./03_intermediate/01_data_retrieval/constr_min/comtrade_cement_trade.csv", delim = ",")

sand_trade <- read_delim("./03_intermediate/01_data_retrieval/constr_min/comtrade_sand_trade.csv", delim = ",")

gravel_trade <- read_delim("./03_intermediate/01_data_retrieval/constr_min/comtrade_gravel_trade.csv", delim = ",")

bricks <- read_delim("./03_intermediate/01_data_retrieval/constr_min/unsd_brick_production.txt", delim = ";")

tiles <- read_delim("./03_intermediate/01_data_retrieval/constr_min/unsd_clay_tile_production.txt", delim = ";")


# iea bitumen
source("./02_scripts/00_functions/lastfile.R")

lastfile <- my_lastfile("./03_intermediate/01_data_retrieval/iea/", "WBES_")

bitumen <- read_delim(
  lastfile,
  delim = ",",
  col_names  = TRUE,
  col_types = cols(Time = "i", Value = "n"),
  locale = locale(encoding = "UTF-8")
)




# concordance tables
source_cou_ids <- read_delim("./01_input/01_concordance_tables/source_country_ids.csv", delim = ";")

source_mat_ids <- read_delim("./01_input/01_concordance_tables/source_material_ids.csv", delim = ";")

source_unit_ids <- read_delim("./01_input/01_concordance_tables/source_unit_ids.csv", delim = ";")








#### harmonize ------

### cement production ----

# clean/filter/format
cement_prod <- cement_prod %>%
  filter(Unit == "Thousand metric tons") %>%
  select(-`Value Footnotes`) %>%
  rename(
    country_code = `Country or Area Code`,
    year = Year,
    unit_id = Unit,
    value = Value
    ) %>%
  mutate(across(country_code, as.character))



## add country_ids
cement_prod <- cement_prod %>% 
  left_join(.,
            source_cou_ids %>% 
              filter(source == "UNSD_cement") %>% 
              select(-source),
            by = c("country_code" = "source_country_id")
  )

# no_match table
no_match_cou <- cement_prod %>%
  filter(is.na(alphanumiso)) %>%
  distinct(country_code, `Country or Area`)



## adjust units
cement_prod <- cement_prod %>%
  mutate(unit_id = "t") %>%
  mutate(value = value * 1000)


# remove NAs
cement_prod <- cement_prod %>%
  filter(!is.na(alphanumiso))


# aggregate
cement_prod <- cement_prod %>%
  filter(!is.na(value)) %>%
  group_by(alphanumiso, year, unit_id) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()







### harmonize USGS cement ----

## format

# wide format
col_names <- names(usgs_cem)[!names(usgs_cem) %in% c("Country")]

usgs_cem <- usgs_cem %>%
  pivot_longer(.,
               cols = !!col_names,
               names_to = "year",
               values_to = "value"
  )

usgs_cem <- usgs_cem %>%
  mutate(
    across(year, as.integer),
    across(value, as.double)
  ) %>%
  filter(!is.na(value))


### add IDs

## add country_ids
usgs_cem <- usgs_cem %>% 
  left_join(.,
            source_cou_ids %>% 
              filter(source == "USGS") %>% 
              select(-source),
            by = c("Country" = "source_country_id")
  )

## no_match table
no_match_cou <- usgs_cem %>%
  filter(is.na(alphanumiso)) %>%
  distinct(Country)


## add unit_ids and convert
  ## All in Kt
usgs_cem <- usgs_cem %>%
  mutate(unit_id = "Kt") %>%
  mutate(value = value * 1000) %>%
  mutate(unit_id = "t")


# remove NAs and obsolete columns
usgs_cem <- usgs_cem %>%
  filter(!is.na(alphanumiso)) %>%
  select(-Country)




### Integrate both cement files (i.e. unsd with usgs, from the latter only entries not covered by unsd)
cement_prod <- cement_prod %>%
  mutate(across(year, as.integer))

temp_1 <- setdiff(
  usgs_cem %>%
    select(alphanumiso, year),
  cement_prod %>%
    select(alphanumiso, year)
  ) %>%
  left_join(., usgs_cem)

cement_prod <- cement_prod %>%
  union(., temp_1)






### cement trade harmonization ----
cement_trade <- cement_trade %>%
  select(Year, `Trade Flow`, Reporter, `Reporter Code`, `Qty Unit`, `Qty`) %>%
  rename(
    year = Year,
    flow = `Trade Flow`,
    country = Reporter,
    country_code = `Reporter Code`,
    unit_id = `Qty Unit`,
    value = `Qty`
    ) %>%
  mutate(across(country_code, as.character)) %>%
  filter(unit_id == "Weight in kilograms")

## add country_ids
cement_trade <- cement_trade %>% 
  left_join(.,
            source_cou_ids %>% 
              filter(source == "Comtrade_cement") %>% 
              select(-source),
            by = c("country_code" = "source_country_id")
  )

# no_match table
no_match_cou <- cement_trade %>%
  filter(is.na(alphanumiso)) %>%
  distinct(country, country_code)



# adjust units
cement_trade <- cement_trade %>%
  mutate(unit_id = "t") %>%
  mutate(value = value * 0.001)


# remove NAs
cement_trade <- cement_trade %>%
  filter(!is.na(alphanumiso))


# aggregate
cement_trade <- cement_trade %>%
  filter(!is.na(value)) %>%
  group_by(alphanumiso, year, unit_id, flow) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()


# correct cement trade for wrong values
cement_trade <- cement_trade %>%
  mutate(value = ifelse(
    alphanumiso == "GMB270" & year == 2015 & flow == "Import",
    value * 0.1,
    value
  ))





### sand trade harmonization ----
sand_trade <- sand_trade %>%
  select(Year, `Trade Flow`, Reporter, `Reporter Code`, `Qty Unit`, `Qty`) %>%
  rename(
    year = Year,
    flow = `Trade Flow`,
    country = Reporter,
    country_code = `Reporter Code`,
    unit_id = `Qty Unit`,
    value = `Qty`
  ) %>%
  mutate(across(country_code, as.character)) %>%
  filter(unit_id == "Weight in kilograms")

## add country_ids
sand_trade <- sand_trade %>% 
  left_join(.,
            source_cou_ids %>% 
              filter(source == "Comtrade_cement") %>% 
              select(-source),
            by = c("country_code" = "source_country_id")
  )

# no_match table
no_match_cou <- sand_trade %>%
  filter(is.na(alphanumiso)) %>%
  distinct(country, country_code)



# adjust units
sand_trade <- sand_trade %>%
  mutate(unit_id = "t") %>%
  mutate(value = value * 0.001)


# remove NAs
sand_trade <- sand_trade %>%
  filter(!is.na(alphanumiso))


# aggregate
sand_trade <- sand_trade %>%
  filter(!is.na(value)) %>%
  group_by(alphanumiso, year, unit_id, flow) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()


# correct sand trade for wrong values
sand_trade <- sand_trade %>%
  mutate(value = ifelse(
    alphanumiso == "MOZ508" & year %in% c(2016, 2017) & flow == "Export",
    value * 0.001,
    value
  ))




### gravel trade harmonization ----
gravel_trade <- gravel_trade %>%
  select(Year, `Trade Flow`, Reporter, `Reporter Code`, `Qty Unit`, `Qty`) %>%
  rename(
    year = Year,
    flow = `Trade Flow`,
    country = Reporter,
    country_code = `Reporter Code`,
    unit_id = `Qty Unit`,
    value = `Qty`
  ) %>%
  mutate(across(country_code, as.character)) %>%
  filter(unit_id == "Weight in kilograms")

## add country_ids
gravel_trade <- gravel_trade %>% 
  left_join(.,
            source_cou_ids %>% 
              filter(source == "Comtrade_cement") %>% 
              select(-source),
            by = c("country_code" = "source_country_id")
  )

# no_match table
no_match_cou <- gravel_trade %>%
  filter(is.na(alphanumiso)) %>%
  distinct(country, country_code)



# adjust units
gravel_trade <- gravel_trade %>%
  mutate(unit_id = "t") %>%
  mutate(value = value * 0.001)


# remove NAs
gravel_trade <- gravel_trade %>%
  filter(!is.na(alphanumiso))


# aggregate
gravel_trade <- gravel_trade %>%
  filter(!is.na(value)) %>%
  group_by(alphanumiso, year, unit_id, flow) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()


# correct gravel trade for wrong values
gravel_trade <- gravel_trade %>%
  mutate(value = ifelse(
    alphanumiso == "HKG344" & year == 2007 & flow == "Export",
    value * 0.1,
    value
  ))





ph <- bricks
bricks <- ph

### brick production harmonization ----

# get only those countries/years for unit "Million units" where no data in cm or t is reported
temp_2 <- setdiff(
  bricks %>%
    filter(Unit %in% c("Million units")) %>%
    distinct(`Country or Area`, Year),
  bricks %>%
    filter(Unit %in% c("Thousand metric tons", "Thousand cubic metres")) %>%
    distinct(`Country or Area`, Year)
  ) %>%
  left_join(.,
            bricks %>%
              filter(Unit %in% c("Million units"))
            )


# clean/filter/format
  # and include temp_2
bricks <- bricks %>%
  filter(Unit %in% c("Thousand metric tons", "Thousand cubic metres")) %>%
  union(., temp_2) %>%
  select(-`Value Footnotes`) %>%
  rename(
    country_code = `Country or Area Code`,
    year = Year,
    unit_id = Unit,
    value = Value
  ) %>%
  mutate(across(country_code, as.character))



## add country_ids
bricks <- bricks %>% 
  left_join(.,
            source_cou_ids %>% 
              filter(source == "UNSD_cement") %>% 
              select(-source),
            by = c("country_code" = "source_country_id")
  )

# no_match table
no_match_cou <- bricks %>%
  filter(is.na(alphanumiso)) %>%
  distinct(country_code, `Country or Area`)


# remove NAs
bricks <- bricks %>%
  filter(!is.na(alphanumiso))


# aggregate
bricks <- bricks %>%
  filter(!is.na(value)) %>%
  group_by(alphanumiso, year, unit_id) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()




ph <- tiles
tiles <- ph


### clay tile production harmonization ----

# get only those countries/years for unit "Million units" where no data in cm or t is reported
  # no double counting shown in data, but this is still done dynamically in case of future updates
temp_3 <- setdiff(
  tiles %>%
    filter(Unit %in% c("Million units")) %>%
    distinct(`Country or Area`, Year),
  tiles %>%
    filter(Unit %in% c("Thousand metric tons")) %>%
    distinct(`Country or Area`, Year)
  ) %>%
  left_join(.,
            tiles %>%
              filter(Unit %in% c("Million units"))
            )


# clean/filter/format
  # and include temp_3
tiles <- tiles %>%
  filter(Unit %in% c("Thousand metric tons")) %>%
  union(., temp_3) %>%
  select(-`Value Footnotes`) %>%
  rename(
    country_code = `Country or Area Code`,
    year = Year,
    unit_id = Unit,
    value = Value
  ) %>%
  mutate(across(country_code, as.character))


## add country_ids
tiles <- tiles %>% 
  left_join(.,
            source_cou_ids %>% 
              filter(source == "UNSD_cement") %>% 
              select(-source),
            by = c("country_code" = "source_country_id")
            )

# no_match table
no_match_cou <- tiles %>%
  filter(is.na(alphanumiso)) %>%
  distinct(country_code, `Country or Area`)


# remove NAs
tiles <- tiles %>%
  filter(!is.na(alphanumiso))


# aggregate
tiles <- tiles %>%
  filter(!is.na(value)) %>%
  group_by(alphanumiso, year, unit_id) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()







### bitumen harmonization ----

# clean/filter/format
bitumen <- bitumen %>% 
  select(-c("PRODUCT", "FLOW", "TIME", "Flag Codes", "Flags")) %>%
  rename(
    "year" = "Time",
    "value" = "Value",
    "flow" = "Flow") %>%
  filter(Product == "Bitumen (kt)")


# add country_ids
bitumen <- bitumen %>% 
  left_join(.,
            source_cou_ids %>% 
              filter(source == "IEA_Code") %>% 
              select(-source),
            by = c("COUNTRY" = "source_country_id")
  )

# no_match table
no_match_cou <- bitumen %>%
  filter(is.na(alphanumiso)) %>%
  distinct(COUNTRY, Country)



# adjust units
bitumen <- bitumen %>%
  mutate(unit_id = "t") %>%
  mutate(value = value * 1000)
  


# remove NAs
bitumen <- bitumen %>%
  filter(!is.na(alphanumiso))



# aggregate
bitumen <- bitumen %>%
  filter(!is.na(value)) %>%
  group_by(alphanumiso, year, unit_id, flow) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()











#### calculations ----

### cement est ----

# combine
cement <- cement_prod %>%
  mutate(flow = "Production") %>%
  union(., cement_trade)


# reduce to maximum year of production
  # (i.e. to not have false estimations in the latest available year based only on trade where production should exist)
  # (i.e. for earlier years dmc is calculated only based on trade if no production exists, because that's simply the case for many countries)
  # (for countries, where no production is available in earlier year, but only trade, also for the latest years numbers are included based only on trade, i.e. no maximum year here)
cement <- cement %>%
  filter(
    year <= (cement %>%
               filter(flow == "Production") %>%
               select(year) %>%
               max()
             )
    )


# format
cement <- cement %>%
  pivot_wider(.,
              names_from = flow,
              values_from = value
              )

# adjust all NAs to 0, for production and trade (in order to avoid DMC being NA)
  # has to be done in wide format, because NAs are introduced during formatting to wide table, when one of the variables is missing
cement <- cement %>%
  mutate(
    Production = ifelse(
      is.na(Production),
      0,
      Production
      )
    ) %>%
  mutate(
    Export = ifelse(
      is.na(Export),
      0,
      Export
    )
  ) %>%
  mutate(
    Import = ifelse(
      is.na(Import),
      0,
      Import
    )
  )

# fill for cement
# cement <- cement %>%
#   arrange(alphanumiso, year) %>%
#   group_by(alphanumiso) %>%
#   tidyr::fill(Production) %>%
#   tidyr::fill(Export) %>%
#   tidyr::fill(Import) %>%
#   ungroup()

# dmc
cement <- cement %>%
  mutate(dmc = Production + Import - Export) %>%
  filter(!is.na(dmc)) %>%
  filter(dmc >= 0) %>%
  select(alphanumiso, year, unit_id, dmc)



# estimation
cement <- cement %>%
  mutate(
    M.lim = dmc * 1.216,
    M.coc = dmc * 0.304,
    M.gya = dmc * 0.05,
    M.sag = dmc * 5.26
  ) %>%
  select(-dmc) %>%
  pivot_longer(.,
               cols = c("M.lim", "M.coc", "M.gya", "M.sag"),
               names_to = "material_id",
               values_to = "value"
               )





### bricks est ----

# estimation
  # values in cm or t only given in either unit, so not double counting and therefore no additional filtering necessary
  # factor for "mio units" is based on comparison of data and represents a very conservative estimate
    # chosen to underestimate significantly, because sample data is very patchy
    # this value has shown to only overestimate in 5% of the cases and underestimated in the other 95%

bricks <- bricks %>%
  mutate(value = case_when(
    unit_id == "Thousand metric tons" ~ (value * 1.349) * 1000, # factor from UN Manual
    unit_id == "Thousand cubic metres" ~ (value * 0.998) * 1000,
    unit_id == "Million units" ~ value * 1800 * 0.998
    )
  ) %>%
  mutate(material_id = "M.coc") %>%
  mutate(unit_id = "t") 





### clay tiles est ----

# estimation
  # values in "mio units" or t only given in either unit, so not double counting and therefore no filtering necessary
  # factor for "mio units" is based on comparison of data and represents a very conservative estimate
    # chosen to underestimate significantly, because sample data is very patchy
    # this value has shown to only overestimate in 5% of the cases and underestimated in the other 95%

tiles <- tiles %>%
  mutate(value = case_when(
    unit_id == "Thousand metric tons" ~ (value * 1.349) * 1000, # factor from UN Manual
    unit_id == "Million units" ~ value * 1000000 * 0.003197 # factor from UN Manual
  )
  ) %>%
  mutate(material_id = "M.coc") %>%
  mutate(unit_id = "t") 








### bitumen est ----

# format
bitumen_est <- bitumen %>%
  pivot_wider(.,
              names_from = flow,
              values_from = value
  )


# fill NAs for latest year(s)
  # adjustment of other NAs not necessary, because not existing in IEA data (i.e. already 0 instead of NA)
bitumen_est <- bitumen_est %>%
  arrange(alphanumiso, year) %>%
  group_by(alphanumiso) %>%
  tidyr::fill(Production) %>%
  tidyr::fill(Exports) %>%
  tidyr::fill(Imports) %>%
  ungroup()



# dmc
bitumen_est <- bitumen_est %>%
  mutate(dmc = Production + Imports + Exports) %>%
  filter(!is.na(dmc)) %>%
  filter(dmc >= 0) %>%
  select(alphanumiso, year, unit_id, dmc)



# estimation
bitumen_est <- bitumen_est %>%
  mutate(
    M.sag = dmc * 51.12
  ) %>%
  select(-dmc) %>%
  pivot_longer(.,
               cols = c("M.sag"),
               names_to = "material_id",
               values_to = "value"
  )







#### Integrate/combine data ----

### Ensure same time coverage for data points which are combined (i.e. M.coc and M.sag)
  ### i.e. extend the respective other data to each country's maximum year

# Crop all estimation to start from 2011
  # in order to not extend any years before that
  # and because all years up to 2011 are included from former db's estimations

cement <- cement %>% filter(year >= 2011)
bricks <- bricks %>% filter(year >= 2011)
tiles <- tiles %>% filter(year >= 2011)
bitumen_est <- bitumen_est %>% filter(year >= 2011)



## M.coc (cement, bricks, tiles)

# maximum years of all three datasets combined
year_max_coc <- cement %>%
  filter(material_id == "M.coc") %>%
  select(alphanumiso, year) %>%
  union(.,
        bricks %>%
          select(alphanumiso, year)
        ) %>%
  union(.,
        tiles %>%
          select(alphanumiso, year)
  ) %>%
  group_by(alphanumiso) %>%
  summarise(y_max = max(year, na.rm = TRUE)) %>%
  ungroup()


# get tibble with all required years
all_years_coc <- tibble(alphanumiso = NA, year = NA) %>%
  mutate(across(alphanumiso, as.character)) %>%
  mutate(across(year, as.integer)) %>%
  filter(!is.na(year))

for (i in 1:nrow(year_max_coc)) {
  
  a <- tibble(
    alphanumiso = year_max_coc$alphanumiso[i],
    year = c(2011:year_max_coc$y_max[i])
  )
  
  all_years_coc <- all_years_coc %>%
    union(., a)
  }


# extend M.coc time series for cement estimations
temp_4 <- all_years_coc %>%
  left_join(.,
            cement %>%
              filter(material_id == "M.coc")
            ) %>%
  arrange(alphanumiso, year) %>%
  group_by(alphanumiso) %>%
  tidyr::fill(value, .direction = "downup") %>%
  ungroup() %>%
  mutate(
    material_id = "M.coc",
    unit_id = "t"
  )

# re-combine with cement est
cement <- cement %>%
  union(., temp_4) %>%
  filter(!is.na(value))



# extend M.coc time series for bricks estimations
  # set 0 to NA and fill, because data shows that even when 0 is reported, production took place, but was only reported in USD
temp_5 <- all_years_coc %>%
  left_join(., bricks) %>%
  mutate(
    value = ifelse(
      value == 0,
      NA,
      value
    )
  ) %>%
  arrange(alphanumiso, year) %>%
  group_by(alphanumiso) %>%
  tidyr::fill(value, .direction = "downup") %>%
  ungroup() %>%
  mutate(
    material_id = "M.coc",
    unit_id = "t"
  )

# re-combine with brick est
bricks <- bricks %>%
  union(., temp_5) %>%
  filter(!is.na(value))



# extend M.coc time series for tiles estimations
  # set 0 to NA and fill, because data shows that even when 0 is reported, production took place, but was only reported in USD
temp_6 <- all_years_coc %>%
  left_join(., tiles) %>%
  mutate(
    value = ifelse(
      value == 0,
      NA,
      value
    )
  ) %>%
  arrange(alphanumiso, year) %>%
  group_by(alphanumiso) %>%
  tidyr::fill(value, .direction = "downup") %>%
  ungroup() %>%
  mutate(
    material_id = "M.coc",
    unit_id = "t"
  )

# re-combine with brick est
tiles <- tiles %>%
  union(., temp_6) %>%
  filter(!is.na(value))




## M.sag (cement, bitumen)

# maximum years of both datasets combined
year_max_sag <- cement %>%
  filter(material_id == "M.sag") %>%
  select(alphanumiso, year) %>%
  union(.,
        bitumen_est %>%
          select(alphanumiso, year)
  ) %>%
  group_by(alphanumiso) %>%
  summarise(y_max = max(year, na.rm = TRUE)) %>%
  ungroup()


# get tibble with all required years
all_years_sag <- tibble(alphanumiso = NA, year = NA) %>%
  mutate(across(alphanumiso, as.character)) %>%
  mutate(across(year, as.integer)) %>%
  filter(!is.na(year))

for (i in 1:nrow(year_max_sag)) {
  
  a <- tibble(
    alphanumiso = year_max_sag$alphanumiso[i],
    year = c(2011:year_max_sag$y_max[i])
  )
  
  all_years_sag <- all_years_sag %>%
    union(., a)
}



# extend M.sag time series for cement estimations
temp_7 <- all_years_sag %>%
  left_join(.,
            cement %>%
              filter(material_id == "M.sag")
  ) %>%
  arrange(alphanumiso, year) %>%
  group_by(alphanumiso) %>%
  tidyr::fill(value, .direction = "downup") %>%
  ungroup() %>%
  mutate(
    material_id = "M.sag",
    unit_id = "t"
  )

# re-combine with cement est
cement <- cement %>%
  union(., temp_7) %>%
  filter(!is.na(value))



# extend M.sag time series for bitumen estimations
temp_8 <- all_years_sag %>%
  left_join(., bitumen_est) %>%
  arrange(alphanumiso, year) %>%
  group_by(alphanumiso) %>%
  tidyr::fill(value, .direction = "downup") %>%
  ungroup() %>%
  mutate(
    material_id = "M.sag",
    unit_id = "t"
  )

# re-combine with cement est
bitumen_est <- bitumen_est %>%
  union(., temp_8) %>%
  filter(!is.na(value))








# combine estimations ----
  # and filter out 0 values
estimations <- cement %>%
  bind_rows(., bricks) %>% 
  bind_rows(., tiles) %>%
  bind_rows(., bitumen_est) %>%
  group_by(alphanumiso, material_id, year, unit_id) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(value > 0)






### combine current estimations and former estimations ----

# remove M.coc for CHN, IND
  # because new estimates (due to missing bricks) or reported data too low and therefore incorrect
  # needs to be filled with former estimations (which used national statistics)
estimations <- estimations %>%
  filter(!(alphanumiso %in% c("CHN156", "IND356") & material_id %in% c("M.coc")))



# all current estimations (starting from 2011) + all former estimations (ending in 2011)
estimations_all <- estimations %>%
  mutate(across(year, as.integer)) %>%
  filter(year > 2011)# %>%
  # union(.,
  #       former_constr %>%
  #         filter(year <= 2011)
  #       )


## extend former estimations for specific countries

# get lowest year of all maximum year for each material (in order to extend former estimations for countries not included in current estimations)
ext_year <- estimations %>%
  group_by(material_id) %>%
  summarise(y_max = max(year)) %>%
  select(y_max) %>%
  min()

# create tibble with years
temp <- tibble(
  temp = "temp",
  year = c(2011:ext_year)
  )

# # get all cases which should be extended
# extend <- setdiff(
#   former_constr %>%
#     filter(value > 0) %>%
#     filter(year == 2011) %>%
#     select(alphanumiso, material_id),
#   estimations_all %>%
#     filter(year > 2011) %>%
#     select(alphanumiso, material_id)
#   )
# 
# # extend
# extend <- extend %>%
#   mutate(temp = "temp") %>%
#   left_join(., temp) %>%
#   left_join(., former_constr) %>%
#   mutate(unit_id = "t") %>%
#   select(-temp) %>%
#   tidyr::fill(value)
# 
# # add to combined estimations
# estimations_all <- estimations_all %>%
#   union(., extend)







### correct estimations for sand and gravel with respective trade data ----
  ### (important for countries like Singapore) 
  ### including former estimations, as it seems that they were not corrected for trade data
sag_trade <- sand_trade %>%
  bind_rows(., gravel_trade) %>%
  group_by(alphanumiso, year, unit_id, flow) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()


# combine with estimations for M.sag
  # and remove NAs and 0s for DMC, in order to not get any wrong new estimations only based on trade values
  # and ensure that coverage of trade is the same, i.e. gap filling everything which doesn't have a trade value
  # otherwise there would be huge jumps in resulting data
  # however, only use interpolation for gap filling and only extrapolate with same value for latest years (i.e. down)
    # extrapolation for earlier has been discarded (check revealed that it creates misleading results)
est_sag <- estimations_all %>%
  filter(material_id == "M.sag") %>%
  mutate(flow = "DMC") %>%
  select(-material_id) %>%
  union(., sag_trade) %>%
  pivot_wider(.,
              names_from = flow,
              values_from = value
              ) %>%
  filter(!is.na(DMC)) %>%
  filter(DMC != 0) %>%
  arrange(alphanumiso, year) %>%
  group_by(alphanumiso) %>%
  mutate(Export = baytrends::fillMissing(Export, span = 5, Dates = year, max.fill = 20)) %>%
  mutate(Import = baytrends::fillMissing(Import, span = 5, Dates = year, max.fill = 20)) %>%
  tidyr::fill(Export, .direction = "down") %>%
  tidyr::fill(Import, .direction = "down") %>%
  ungroup()





# adjust all left NAs to 0, for trade (in order to avoid DE being NA), while NAs for DMC were already filtered out
  # has to be done in wide format, because NAs are introduced during formatting to wide table, when one of the variables is missing
est_sag <- est_sag %>%
  mutate(
    Export = ifelse(
      is.na(Export),
      0,
      Export
    )
  ) %>%
  mutate(
    Import = ifelse(
      is.na(Import),
      0,
      Import
    )
  )


# correct (i.e. calculate actual DE)
  # and remove resulting negative values
est_sag <- est_sag %>%
  mutate(DE = DMC - Import + Export) %>%
  filter(DE >= 0)



# re-combine with estimations_all
estimations_all <- estimations_all %>%
  filter(material_id != "M.sag") %>%
  union(., 
        est_sag %>%
          select(-DMC,-"Re-Export",-"Re-Import" ,-Export, -Import) %>%
          rename(value = DE) %>%
          mutate(material_id = "M.sag")
        )




# include comment on source
estimations_all <- estimations_all %>%
  mutate(
    source_est = case_when(
      material_id == "M.coc" ~ "estimation based on consumption of cement and production of bricks and tiles",
      material_id == "M.gya" ~ "estimation based on consumption of cement",
      material_id == "M.lim" ~ "estimation based on consumption of cement",
      material_id == "M.sag" ~ "estimation based on consumption of cement and bitumen"
      )
    )








### integrate with data ----

# get maximum years for estimated materials (plus respective value)
  # in order to cut off time series after integration
    # in order to avoid drops in time series for most recent years when reported values are available, but are much lower than estimations in previous years
    # however, only, if values are lower than maximum year's estimated value (i.e. keeping reported values which would be higher anyway)

max_year <- estimations_all %>%
  group_by(alphanumiso, material_id) %>%
  summarise(y_max = max(year, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(.,
            estimations_all %>%
              select(-unit_id, -source_est),
            by = c("alphanumiso", "material_id", "y_max" = "year")
            ) %>%
  rename(value_y_max = value)



# remove low reported values from time series after year where estimations end
data <- data %>%
  left_join(., max_year) %>%
  mutate(value = ifelse(
    !is.na(y_max) & year > y_max & !is.na(value_y_max) & value < value_y_max,
    NA,
    value
    )
  ) %>%
  filter(!is.na(value)) %>%
  select(-y_max, -value_y_max)
  




## combine
  # filling value if it is NA and estimated value is available
  # replacing value if it is not NA and estimated value is higher
  # plus adjusting source if estimated value is used

# create table for M.sag and M.crs in order to keep detail
temp <- data %>%
  filter(material_id %in% c("M.sag", "M.crs"))
  # distinct(alphanumiso, year, source) %>%
  # group_by(alphanumiso, year) %>%
  # summarise(n = n()) %>%
  # ungroup() %>%
  # filter(n > 1)

# aggregate data for M.sag and M.crs
  # excluding source, because some countries have data from both, bgs and usgs in the same year
temp_agg <- temp %>%
  mutate(material_id = "M.sag") %>%
  group_by(alphanumiso, material_id, year, unit_id) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(source = "M.sag aggregate")

#### changes due to huge step in values discovered in update 2022
# temp_agg[((temp_agg$alphanumiso=="VNM074")+(temp_agg$material_id=="M.sag"))==2,]

# include aggregate in data instead of detailed values
data <- data %>%
  filter(!material_id %in% c("M.sag", "M.crs")) %>%
  union(., temp_agg)


# integrate
data <- data %>%
  full_join(.,
            estimations_all,
            by = c("alphanumiso", "material_id", "year", "unit_id"),
            suffix = c("", "_est")
  ) %>%
  mutate(
    source = ifelse(
      (is.na(value) & !is.na(value_est)) | (!is.na(value) & !is.na(value_est) & value_est > value),
      source_est,
      source
    )) %>%
  mutate(
    value = ifelse(
      (is.na(value) & !is.na(value_est)) | (!is.na(value) & !is.na(value_est) & value_est > value),
      value_est,
      value
    ))

data <- data %>%
  select(-value_est, -source_est)


# include detail for M.sag and M.crs again
data <- data %>%
  filter(!source == "M.sag aggregate") %>%
  union(., 
        intersect(
          data %>%
            filter(source == "M.sag aggregate") %>%
            distinct(alphanumiso, year),
          temp %>%
            distinct(alphanumiso, year)
        ) %>%
          left_join(., temp)
        )


# save rds (data and estimations)
write_rds(data, "./03_intermediate/4b_data_estimation/data_with_constr.rds")

write_rds(estimations_all, "./04_output/05_supplementary_info/estimated_constr_min.rds")




a <- data %>%
  group_by(year) %>%
  summarise(value = sum(value, na.rm = TRUE))




