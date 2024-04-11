# import necessary packages
library(sf)
library(dplyr)
library(purrr)
library(stringr)
library(janitor)
library(readxl)

## IMPORT
bridge <- readRDS("output/geo/plz_wkr_bridge.rds")

# set column types for import
nrw_col_types <- c(
  "text", "text", "text","date","date","numeric","text","text","text", "text",
  "text","text","numeric","numeric"
)

# import excel
nrw <- read_xlsx("raw/federal_states/NRW.xlsx", skip = 1, 
                 col_types = nrw_col_types) %>%
  clean_names() %>%
  # oddly, some foreign projects being funded — filter out and delete col
  filter(
    country == "DE"
  ) %>%
  select(-country) %>%
  # rename columns
  rename(
    operation = operation_name,
    purpose = purpose_and_expected_achievements,
    cost = total_cost,
    objective = specific_objective,
    eu_cofinancing = union_co_financing_rate,
    plz = location_indicator_postal_code,
    city = location_indicator_location,
    intervention_type = interven_tion_type
  ) %>%
  # remove blank spaces and get rid of blank spaces
  mutate(
    plz = str_replace_all(plz, " ", ""),
    funding_state = "Nordrhein-Westfalen"
  )

# delete blank rows
nrw <- nrw[-c(1, 2), ]

# note: many-to-many is required (multiple PLZ have same BWK,
# some BWK have multiple PLZ)
nrw_wk <- nrw %>%
  left_join(bridge, join_by(plz), relationship = "many-to-many")
