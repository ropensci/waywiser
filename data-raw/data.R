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

## code to prepare `bioclim_simulation` dataset goes here
# Code adapted from https://hannameyer.github.io/CAST/articles/cast02-AOA-tutorial.html
set.seed(123)

library(CAST)
library(virtualspecies)

n_sample_points <- 10000
predictors <- raster::brick(
  system.file(
    "extdata/bioclim_global.grd",
    package = "CAST"
  )
)

response <- generateSpFromPCA(
  predictors,
  means = c(3, -1),
  sds = c(2, 2),
  plot = FALSE
)$suitab.raster

mask <- predictors[[1]]
values(mask)[!is.na(values(mask))] <- 1
mask <- rasterToPolygons(mask, dissolve = TRUE)
samplepoints <- spsample(mask, n_sample_points, "random")

worldclim_simulation <- extract(predictors, samplepoints, sp = TRUE)
worldclim_simulation <- sf::st_as_sf(worldclim_simulation)
worldclim_simulation$response <- extract(response, samplepoints)
worldclim_simulation <- na.omit(worldclim_simulation)

usethis::use_data(guerry, ny_trees, worldclim_simulation, internal = TRUE, overwrite = TRUE)
