# import necessary packages
library(sf)
library(dplyr)
library(purrr)
library(stringr)
library(janitor)

# data from the federal statistical office that purportedly
# joins PLZ and BWK data is missing most PLZs, since it only includes
# the main PLZ for each Gemeinde (and its associated BWK). see data at:
# https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/Archiv/GVAuszugQ/BTW20214Q2020.html
# and see FOIA request and explanation at:
# https://fragdenstaat.de/anfrage/veroffentlichung-einer-vollstandigen-liste-von-postleitzahlen-und-den-zugehorigen-wahlkreisen/

## DATA IMPORT

# plz data from https://www.suche-postleitzahl.org/downloads
plz <- st_read("raw_data/shape_files/plz5.geojson")

# bwk data from https://www.bundeswahlleiterin.de/en/bundestagswahlen/2017/wahlkreiseinteilung/downloads.html
# converted using https://www.shptogeojson.com/
bwk <- st_read("raw_data/shape_files/wkr.json")

## FULLY CONTAINED DISTRICTS

# first, come up with a list of all PLZs that are fully enclosed within BWKs
# 2824 PLZs -- not bad!
full_contains <- st_join(bwk, plz, join = st_contains_properly) %>%
  clean_names() %>%
  select(plz, note, everything()) %>%
  rename(
    plz_name = note
  ) %>%
  mutate(
    plz_name = str_replace(plz_name, paste(plz, " ", sep = ""), ""),
    plz_name = str_replace(plz_name, "  ", " "),
    wk_pct = 1
  ) %>%
  st_drop_geometry()

# filter out the list of remaining PLZs
plz <- plz %>%
  filter(!plz %in% full_contains$plz)

## PARTIALLY CONTAINED DISTRICTS
# use the `st` function for overlapping districts to find all BWK that
# are in a given PLZ

partial_matches <- st_join(plz, bwk, join = st_overlaps)

# first, taking care of a bit of an oddity here!
augsburg_plz <- c("82297", "86154", "86156", "86316", "86356", "86368", "86391", "86399", "86405", "86415", "86420", "86424", "86438", "86441", "86444", "86465", "86485", "86494", "86495", "86504", "86507", "86508", "86510", "86511", "86551", "86559", "86568", "86577", "86672", "86678", "86679", "86695", "86707", "86830", "86836", "86850", "86853", "86856", "86863", "86868", "86872", "86877")

# will join this data back in later xoxo
augsburg_oddity <- partial_matches %>%
  filter(WKR_NR == 253 & plz %in% augsburg_plz) %>%
  clean_names() %>%
  select(plz, note, wkr_nr, wkr_name, land_nr, land_name, einwohner, qkm, geometry) %>%
  rename(plz_name = note) %>%
  mutate(
    plz_name = str_replace(plz_name, paste(plz, " ", sep = ""), ""),
    plz_name = str_replace(plz_name, "  ", " "),
    wk_pct = NA
  ) %>%
  st_drop_geometry()

# arrange by number for easier tracking
partial_matches <- partial_matches %>%
  arrange(plz) %>%
  # weird glitch  with augsburg-land // manually edit later
  filter(!WKR_NR == 253)

# setting a function to find shared area for each row
calculate_intersection_area <- function(plz_code, wahlkreis_nummer) {
  plz_geom <- plz %>%
    filter(plz == plz_code) %>%
    st_geometry()
  bwk_geom <- bwk %>%
    filter(WKR_NR == wahlkreis_nummer) %>%
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
partial_matches$intersection_area <- pmap_dbl(list(partial_matches$plz, partial_matches$WKR_NR), calculate_intersection_area)

partial_matches <- partial_matches %>%
  # finding the area of the plz itself
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

# apply tolerance
partial_matches <- partial_matches %>%
  filter(wk_pct > .25) %>%
  clean_names() %>%
  select(plz, note, wkr_nr, wkr_name, land_nr, land_name, einwohner, qkm, wk_pct) %>%
  rename(plz_name = note) %>%
  mutate(
    plz_name = str_replace(plz_name, paste(plz, " ", sep = ""), ""),
    plz_name = str_replace(plz_name, "  ", " ")
  )

# 95 PLZ have multiple WK associated with them
repeat_num <- partial_matches[c("plz", "wkr_nr")] %>%
  group_by(plz) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(count) %>%
  summarise(n())

# time for the big bad boy
final_df <- rbind(rbind(full_contains, partial_matches), augsburg_oddity) %>%
  filter(!is.na(plz)) %>%
  arrange(plz)

# reset row names
row.names(final_df) <- NULL

# tada!!
write.csv(final_df, "output_data/plz_wkr_bridge.csv")
