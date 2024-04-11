# import necessary packages
library(sf)
library(dplyr)
library(purrr)
library(stringr)
library(janitor)

# Data from the Federal Statistical Office that only includes the main PLZ for each Gemeinde
# and its associated BWK is missing most PLZs. This script aims to refine and bridge gaps
# in this data by mapping PLZs to BWKs accurately.
# Source data links:
# Inadequate data:
# https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/Archiv/GVAuszugQ/BTW20214Q2020.html
# Wahlkreis names (corrupted in import) + geoJSON
# https://www.bundeswahlleiterin.de/en/bundestagswahlen/2021/wahlkreiseinteilung/downloads.html
# PLZ geoJSON
# https://www.suche-postleitzahl.org/downloads
# FOIA request and explanation:
# https://fragdenstaat.de/anfrage/veroffentlichung-einer-vollstandigen-liste-von-postleitzahlen-und-den-zugehorigen-wahlkreisen/

## DATA IMPORT
# Load PLZ shapefiles
plz <- st_read("raw/shape_files/plz5.geojson") %>%
  clean_names() %>%
  select(plz, note, everything()) %>%
  rename(
    plz_name = note,
    plz_einwohner = einwohner,
    plz_qkm = qkm
  ) %>%
  mutate(
    plz_name = str_replace(plz_name, paste(plz, " ", sep = ""), ""),
    plz_name = str_replace(plz_name, "  ", " ")
  ) %>%
  filter(
    plz_einwohner > 10
  )

# Load Wahlkreis names
wk_namen <- read.csv("raw/wahlkreisnamen.csv", skip = 7) %>%
  select(-LAND_NR, -LAND_ABK)

# Load BWK data
bwk <- st_read("raw/shape_files/wkr.json") %>%
  select(WKR_NR, LAND_NR) %>%
  inner_join(wk_namen, by = c("WKR_NR")) %>%
  clean_names() %>%
  select(wkr_nr, wkr_name, land_nr, land_name)

## FULLY CONTAINED DISTRICTS
# Identify all PLZs that are fully enclosed within BWKs (obs: 2815)

full_contains <- st_join(bwk, plz, join = st_contains_properly) %>%
  mutate(wk_pct = 1) %>%
  st_drop_geometry()

# filter out the list of remaining PLZs
plz <- plz %>%
  filter(!plz %in% full_contains$plz)

## PARTIALLY CONTAINED DISTRICTS
# Identify PLZs that partially overlap multiple BWKs

partial_matches <- st_join(plz, bwk, join = st_overlaps)

# Manually coding out Augsburg 253 exceptions
augsburg_plz <- c("82297", "86154", "86156", "86316", "86356", "86368", "86391", "86399", "86405", "86415", "86420", "86424", "86438", "86441", "86444", "86465", "86485", "86494", "86495", "86504", "86507", "86508", "86510", "86511", "86551", "86559", "86568", "86577", "86672", "86678", "86679", "86695", "86707", "86830", "86836", "86850", "86853", "86856", "86863", "86868", "86872", "86877")
augsburg_oddity <- partial_matches %>%
  filter(wkr_nr == 253 & plz %in% augsburg_plz) %>%
  select(plz, plz_name, wkr_nr, wkr_name, land_nr, land_name, plz_einwohner, plz_qkm, geometry) %>%
  mutate(
    wk_pct = NA
  ) %>%
  st_drop_geometry()

# arrange by number for easier tracking
partial_matches <- partial_matches %>%
  arrange(plz) %>%
  filter(!wkr_nr == 253)

# setting a function to find shared area for each row
calculate_intersection_area <- function(plz_code, wahlkreis_nummer) {
  plz_geom <- plz %>%
    filter(plz == plz_code) %>%
    st_geometry()
  bwk_geom <- bwk %>%
    filter(wkr_nr == wahlkreis_nummer) %>%
    st_geometry()
  intersection_geom <- st_intersection(plz_geom, bwk_geom)
  if (!is.null(intersection_geom)) {
    cat(paste(plz_code, ".", sep = ""))
    return(st_area(intersection_geom))
  } else {
    return(0)
  }
}

# time for some base r baby! calculating the areas of the intersections purrr
partial_matches$intersection_area <- pmap_dbl(list(partial_matches$plz, partial_matches$wkr_nr), calculate_intersection_area)

partial_matches <- partial_matches %>%
  # finding the area of the PLZ itself
  mutate(
    plz_area = as.numeric(str_replace(st_area(geometry), " \\[m\\^2\\]", "")),
    wk_pct = intersection_area / plz_area
  ) %>%
  st_drop_geometry()

# function to test tolerance
nrows_partial <- function(tolerance) {
  df <- partial_matches %>%
    filter(wk_pct > as.numeric(tolerance))
  return(nrow(df))
}

# creating table of tolerances -- it seems that .25 is a reasonable threshold
sig_table <- tibble(
  tolerance = c(0, .01, .05, .1, .15, .2, .25, .33, .4, .5),
)
sig_table$nrows <- map(sig_table$tolerance, nrows_partial)

saveRDS(sig_table, file = "output_data/sig_table.rds")

# apply tolerance
partial_matches <- partial_matches %>%
  filter(wk_pct > .25) %>%
  clean_names() %>%
  select(plz, plz_name, wkr_nr, wkr_name, land_nr, land_name, plz_einwohner, plz_qkm, wk_pct)

# 94 PLZ have multiple WK associated with them
repeat_num <- partial_matches[c("plz", "wkr_nr")] %>%
  group_by(plz) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(count) %>%
  summarise(n())

# time for the big bad boy
final_df <- rbind(rbind(full_contains, partial_matches), augsburg_oddity) %>%
  filter(!is.na(plz)) %>%
  arrange(plz) %>%
  select(plz, plz_name, wkr_nr, wkr_name, land_nr, land_name, plz_einwohner, plz_qkm, wk_pct)

# reset row names
row.names(final_df) <- NULL

# tada!!
library(openxlsx)
write.xlsx(final_df, "output/geo/plz_wkr_bridge.xlsx")
write.csv(final_df, "output/geo/plz_wkr_bridge.csv", fileEncoding = "UTF-8")
saveRDS(final_df, file = "output/geo/plz_wkr_bridge.rds")
