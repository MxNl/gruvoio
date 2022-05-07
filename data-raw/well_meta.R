## code to prepare `well_meta` dataset goes here
library(dplyr)
library(rnaturalearth)
library(sf)
library(stringr)
library(ggplot2)


germany <- rnaturalearth::ne_countries(
  scale = 50,
  country = "Germany",
  returnclass = "sf"
  ) |>
  st_transform(32632) |>
  select(geometry)

germany_gridpoints <- st_make_grid(
  germany,
  cellsize = 1E4,
  what = "centers",
  square = FALSE
)

set.seed(1)
well_meta <- germany_gridpoints |>
  st_intersection(germany) |>
  st_sample(600) |>
  st_cast("POINT") |>
  st_set_crs(st_crs(germany_gridpoints)) |>
  st_as_sf() |>
  rename(geometry = x)

states_abbrev <- c("BB", "BE", "HE", "TH", "SH", "SN", "ST", "BN", "BW", "SL", "NI", "RP") |>
  rep_len(length.out = nrow(well_meta)) |>
  sort()

well_ids <- states_abbrev |>
  str_c(1E5:(1E5 + nrow(well_meta) - 1), sep = "_")

set.seed(1)
well_ids_ref <- well_ids |>
  sample(30)

well_meta <- well_meta |>
  mutate(well_id = well_ids, .before = 1) |>
  mutate(rm_cm = if_else(well_id %in% well_ids_ref, "rm", "cm"), .after = 1)

well_meta_ref <- well_meta |>
  filter(rm_cm == "rm")

nearest_ref_well <- well_meta_ref |>
  slice(well_meta |> st_nearest_feature(well_meta_ref)) |>
  pull(well_id)

well_meta <- well_meta |>
  mutate(rm = nearest_ref_well, .after = 2)

well_meta <- well_meta |>
  st_transform(4326)

# Add gwl trend
set.seed(1)
well_meta <- well_meta |>
  rowwise() |>
  mutate(
    pred_change = runif(1),
    .before = geometry) |>
  group_by(rm) |>
  mutate(
    pred_trend = sample(c("+", "-", 0), 1, replace = TRUE),
    .before = geometry) |>
  ungroup() |>
  mutate(pred_change = case_when(
    pred_trend == "+" ~ pred_change,
    pred_trend == "-" ~ -pred_change,
    pred_trend == "0" ~ pred_change/100
  )) |>
  mutate(
    pred_gwlnn = pred_change + runif(1, 10, 100),
    .before = geometry)

# germany_gridpoints |>
#   ggplot() +
#   geom_sf(colour = "white", size = 1) +
#   geom_sf(data = well_meta, aes(colour = rm), size = 1) +
#   geom_sf(data = germany, fill = NA, colour = "red")

usethis::use_data(well_meta, overwrite = TRUE)
