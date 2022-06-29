test_that("ww_make_point_neighbors is stable", {

  skip_if_not(rlang::is_installed("sfdep"))
  data(guerry, package = "sfdep")
  guerry_pt <- sf::st_geometry(guerry)
  guerry_pt <- sf::st_centroid(guerry_pt)
  expect_snapshot(
    ww_make_point_neighbors(guerry_pt)
  )

  expect_snapshot(
    ww_make_point_neighbors(guerry_pt, k = 2)
  )

})

test_that("ww_make_polygon_neighbors is stable", {

  skip_if_not(rlang::is_installed("sfdep"))
  data(guerry, package = "sfdep")

  expect_snapshot(
    ww_make_polygon_neighbors(guerry)
  )

  expect_snapshot(
    ww_make_polygon_neighbors(guerry, queen = FALSE)
  )

})

test_that("ww_build_neighbors is stable", {

  skip_if_not(rlang::is_installed("sfdep"))
  data(guerry, package = "sfdep")

  expect_snapshot(
    ww_build_neighbors(guerry)
  )

  expect_identical(
    ww_build_neighbors(sf::st_polygon(), ww_build_neighbors(guerry)),
    ww_build_neighbors(guerry)
  )

  guerry_pt <- sf::st_geometry(guerry)
  guerry_pt <- sf::st_centroid(guerry_pt)
  expect_snapshot(
    ww_build_neighbors(guerry_pt)
  )

  expect_snapshot(
    ww_build_neighbors(sf::st_cast(guerry, "MULTILINESTRING"))
  )

  expect_snapshot(
    ww_build_neighbors(guerry, function(data) data),
    error = TRUE
  )

})

test_that("ww_build_weights is stable", {

  skip_if_not(rlang::is_installed("sfdep"))
  data(guerry, package = "sfdep")

  expect_snapshot(
    ww_build_weights(guerry)
  )

  expect_identical(
    ww_build_weights(guerry, ww_build_weights(guerry)),
    ww_build_weights(guerry)
  )

  guerry_pt <- sf::st_geometry(guerry)
  guerry_pt <- sf::st_centroid(guerry_pt)
  expect_snapshot(
    ww_build_weights(guerry_pt)
  )

  expect_snapshot(
    ww_build_weights(sf::st_cast(guerry, "MULTILINESTRING"))
  )

  expect_snapshot(
    ww_build_weights(guerry, function(data) data),
    error = TRUE
  )

})
