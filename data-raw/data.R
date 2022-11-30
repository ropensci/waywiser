## code to prepare `ny_trees` dataset goes here

# code originally written for
# https://www.tidymodels.org/learn/work/multi-scale/
library(sf)
library(dplyr)

invisible(sf_proj_network(TRUE))

ny_trees <- readr::read_csv("https://www.tidymodels.org/learn/work/multi-scale/plots.csv") |>
  select(-PLOT) |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |>
  st_transform(5070)

## code to prepare `guerry` dataset goes here

guerry <- geodaData::guerry |>
  sf::st_transform(27572)

usethis::use_data(guerry, ny_trees, internal = TRUE, overwrite = TRUE)
