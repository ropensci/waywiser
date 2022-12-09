test_that("ww_multi_scale", {
  skip_if_not_installed("modeldata")
  data(ames, package = "modeldata")
  ames_sf <- sf::st_as_sf(ames, coords = c("Longitude", "Latitude"), crs = 4326)
  ames_model <- lm(Sale_Price ~ Lot_Area, data = ames_sf)
  ames_sf$predictions <- predict(ames_model, ames_sf)

  made_w_grid_args <- ww_multi_scale(
    ames_sf,
    Sale_Price,
    predictions,
    n = list(
      c(20, 20),
      c(10, 10),
      c(1, 1)
    ),
    square = FALSE
  )

  embiggened_bbox <- expand_grid(sf::st_bbox(ames_sf))

  grids <- list(
    sf::st_make_grid(embiggened_bbox, n = c(20, 20), square = FALSE),
    sf::st_make_grid(embiggened_bbox, n = c(10, 10), square = FALSE),
    sf::st_make_grid(embiggened_bbox, n = c(1, 1), square = FALSE)
  )
  made_w_grids <- ww_multi_scale(ames_sf, Sale_Price, predictions, grids = grids)

  expect_identical(
    made_w_grid_args[1:3],
    made_w_grids[1:3]
  )
  expect_snapshot(made_w_grid_args)

  expect_snapshot_warning(
    ww_multi_scale(
      ames_sf,
      Sale_Price,
      predictions,
      n = list(c(1, 1)),
      autoexpand_grid = FALSE
    )
  )

  expect_equal(
    ww_multi_scale(
      ames_sf,
      Sale_Price,
      predictions,
      n = list(c(1, 1)),
      metrics = list(yardstick::mae)
    )$.estimate,
    yardstick::mae_vec(mean(ames_sf$Sale_Price), mean(ames_sf$predictions))
  )

  expect_equal(
    ww_multi_scale(
      ames_sf,
      Sale_Price,
      predictions,
      n = list(c(1, 1)),
      metrics = list(yardstick::mae),
      aggregation_function = median
    )$.estimate,
    yardstick::mae_vec(median(ames_sf$Sale_Price), median(ames_sf$predictions))
  )

})
