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
  "text", "text", "text", "date", "date", "numeric", "text", "text", "text", "text",
  "text", "text", "numeric", "numeric"
)

# import excel
nrw <- read_xlsx("raw/federal_states/NRW.xlsx",
  skip = 1,
  col_types = nrw_col_types
) %>%
  clean_names() %>%
  # oddly, some foreign projects being funded — filter out and delete col
  filter(
    country == "DE"
  ) %>%
  select(-country) %>%
  # rename columns
  rename(
    project_name = operation_name,
    project_purpose = purpose_and_expected_achievements,
    project_cost = total_cost,
    project_start = start_date,
    project_end = end_date,
    eso_objective = specific_objective,
    eu_cofinancing = union_co_financing_rate,
    plz = location_indicator_postal_code,
    city = location_indicator_location,
    intervention_code = interven_tion_type
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
nrw_full <- nrw %>%
  left_join(bridge, join_by(plz), relationship = "many-to-many") %>%
  mutate(
    eu_cofinancing = str_replace(eu_cofinancing, " %", ""),
    eu_cofinancing = as.numeric(eu_cofinancing) / 100,
    eu_cost = round(project_cost * eu_cofinancing, 2)
  ) %>%
  rename(
    project_id = id,
  ) %>%
  select(
    project_id, beneficiary, project_name, project_purpose, eso_objective,
    intervention_code, project_start, project_end, project_cost, eu_cofinancing,
    eu_cost, funding_state, city, plz, everything(), -fund
  )

library(openxlsx)
write.xlsx(nrw_full, "output/state/nrw.xlsx")
write.csv(nrw_full, "output/state/nrw.csv", fileEncoding = "UTF-8")
saveRDS(nrw_full, file = "output/state/nrw.rds")

reactable(nrw_full)

nrw_summary <- nrw_full %>%
  group_by(beneficiary, city, project_name) %>%
  summarize(
    objectives = toString(unique(eso_objective)),
    total_funding = sum(eu_cost),
    plz = toString(unique(plz)),
    wkr_nr = toString(unique(wkr_nr)),
    wkr_name = toString(unique(wkr_name)),
    land = toString(unique(land_name))
  ) %>%
  ungroup() %>%
  select(
    beneficiary, project_name, objectives, total_funding, city, everything()
  )

reactable(nrw_summary,
  groupBy = "beneficiary",
  searchable = TRUE,
  columns = list(
    beneficiary = colDef(name = "Beneficiary", align = "left"),
    city = colDef(name = "City", aggregate = "unique", align = "right"),
    project_name = colDef(name = "Project Name", align = "left"),
    objectives = colDef(name = "Objectives", aggregate = "unique", align = "left"),
    total_funding = colDef(name = "EU Funding", aggregate = "sum", align = "right", format = colFormat(prefix = "€", separators = TRUE, digits = 2, locales = "de-DE")),
    plz = colDef(name = "PLZ", aggregate = "unique", align = "right"),
    wkr_nr = colDef(name = "Electoral District", aggregate = "unique", align = "right"),
    wkr_name = colDef(name = "District Name", aggregate = "unique", align = "right"),
    land = colDef(name = "State", aggregate = "unique", align = "right")
  ),
  defaultPageSize = 10,
  paginationType = "jump",
  highlight = TRUE,
  bordered = TRUE, 
  striped = TRUE
)
