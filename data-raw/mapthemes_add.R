## code to prepare `mapthemes_add` dataset goes here

mapthemes_add <- c("ohne", "HÜK250", "Hydr. Großräume", "Hydr. Räume", "Hydr. Teilräum", "OpenStreetMap")

usethis::use_data(mapthemes_add, overwrite = TRUE)
