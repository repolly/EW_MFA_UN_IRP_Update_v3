#### Additional est factors based on reported data

  #### Currently only for chosen, relevant cases

  # O.P vs Nm.PO
  # O.Li vs Me.Li2O
  # O.Al vs Me.Al



library(tidyverse)


# clear R environment
rm(list = ls())



## read files

wmd <- read_rds("./03_intermediate/04a_data_conversion/conversion_elements/wmd_conversion_1.rds") %>%
  mutate(source = "wmd")

bgs <- read_rds("./03_intermediate/04a_data_conversion/conversion_elements/bgs_conversion_1.rds") %>%
  mutate(source = "bgs")







### Phosphor ----

# filter and combine
data <- wmd %>%
  filter(material_id == "Nm.PO") %>%
  select(alphanumiso, material_id, year, value) %>%
  union(.,
            bgs %>%
              filter(material_id == "O.P") %>%
              select(alphanumiso, material_id, year, value)
            )


# wide format
data <- data %>%
  pivot_wider(
    names_from = material_id,
    values_from = value
  ) %>%
  filter(!is.na(Nm.PO)) %>%
  filter(!is.na(O.P))

# calculate
est_fac <- data %>%
  mutate(factor = Nm.PO/O.P) %>%
  filter(factor != Inf) %>%
  filter(!is.na(factor)) %>%
  filter(factor < 1 & factor > 0) 


est_fac %>%
  select(factor) %>%
  pull() %>%
  mean()







### Lithium ----

# filter and combine
data <- wmd %>%
  filter(material_id == "Me.Li2O") %>%
  select(alphanumiso, material_id, year, value) %>%
  union(.,
        bgs %>%
          filter(material_id == "O.Li") %>%
          select(alphanumiso, material_id, year, value)
  )


# wide format
data <- data %>%
  pivot_wider(
    names_from = material_id,
    values_from = value
  ) %>%
  filter(!is.na(Me.Li2O)) %>%
  filter(!is.na(O.Li))

# calculate
est_fac <- data %>%
  mutate(factor = O.Li/Me.Li2O) %>%
  filter(factor != Inf) %>%
  filter(!is.na(factor)) %>%
  filter(factor > 1)

est_fac_lit <- est_fac %>%
  group_by(alphanumiso) %>%
  summarise(factor = mean(factor, na.rm = TRUE)) %>%
  mutate(material_id = "Me.Li2O")

# remove Argentina, because in latest years sourced from salt brine and not from ore
est_fac_lit <- est_fac_lit %>%
  filter(alphanumiso != "ARG032")


write_delim(est_fac_lit, "./01_input/03_factors/estimation_factors_add_2.csv", delim = ";")







### Bauxite/Aluminium ----

# filter and combine
temp <- setdiff(
  bgs %>%
    filter(material_id %in% c("O.Al", "Me.Al")) %>%
    select(alphanumiso, material_id, year),
  wmd %>%
    filter(material_id %in% c("O.Al", "Me.Al")) %>%
    select(alphanumiso, material_id, year)
  ) %>%
  left_join(., bgs %>% select(alphanumiso, material_id, year, value))

data <- wmd %>%
  filter(material_id %in% c("O.Al", "Me.Al")) %>%
  select(alphanumiso, material_id, year, value) %>%
  union(., temp)



# wide format
data <- data %>%
  pivot_wider(
    names_from = material_id,
    values_from = value
  ) %>%
  filter(!is.na(Me.Al)) %>%
  filter(!is.na(O.Al))

# calculate
est_fac <- data %>%
  mutate(factor = O.Al/Me.Al) %>%
  filter(factor != Inf) %>%
  filter(!is.na(factor)) %>%
  filter(factor > 1)

est_fac_alu <- est_fac %>%
  group_by(alphanumiso) %>%
  summarise(factor = mean(factor, na.rm = TRUE)) %>%
  mutate(material_id = "Me.Al")

write_delim(est_fac_alu, "./01_input/03_factors/estimation_factors_add_3.csv", delim = ";")



