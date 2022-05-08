## code to prepare `well_data` dataset goes here
library(dplyr)
library(purrr)


ref_well_ids <- well_meta |>
  distinct(rm) |>
  rename(well_id = rm) |>
  pull(well_id)

dates <- seq.Date(as.Date("2020-01-01"), Sys.time() |> as.Date(), by = 7)
n_dates <- length(dates)
sinus_wave <- sin(seq(1, 20, length.out = n_dates))

add_noise <- function(x, y) {
  tibble(
    well_id = x,
    gwl = y + runif(length(y))
    )
}

well_data <- ref_well_ids |>
  purrr::map_dfr(add_noise, sinus_wave) |>
  group_by(well_id) |>
  mutate(date = dates, .after = 1)

usethis::use_data(well_data, overwrite = TRUE)
