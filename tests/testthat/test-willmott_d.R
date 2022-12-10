test_that("Willmott's D estimates are the same across methods", {
  x <- c(6, 8, 9, 10, 11, 14)
  y <- c(2, 3, 5, 5, 6, 8)
  df <- data.frame(x = x, y = y)

  sim <- c(5, 7, 9, 2, 4.5, 6.7)
  obs <- c(4.7, 6, 10, 2.5, 4, 7)

  expect_equal(
    ww_willmott_d_vec(y, x),
    0.5137892
  )

  expect_equal(
    ww_willmott_dr_vec(obs, sim),
    0.847457627118644
  )

  expect_equal(
    ww_systematic_mse_vec(y, x) + ww_unsystematic_mse_vec(y, x),
    yardstick::rmse_vec(y, x) ** 2
  )

  expect_equal(
    ww_systematic_rmse_vec(y, x) ** 2 + ww_unsystematic_rmse_vec(y, x) ** 2,
    yardstick::rmse_vec(y, x) ** 2
  )

  expect_equal(
    ww_willmott_d_vec(x, y),
    ww_willmott_d(df, x, y)$.estimate
  )

  expect_equal(
    ww_willmott_dr_vec(x, y),
    ww_willmott_dr(df, x, y)$.estimate
  )

  expect_equal(
    ww_systematic_mse_vec(x, y),
    ww_systematic_mse(df, x, y)$.estimate
  )

  expect_equal(
    ww_unsystematic_mse_vec(x, y),
    ww_unsystematic_mse(df, x, y)$.estimate
  )

  expect_equal(
    ww_systematic_rmse_vec(x, y),
    ww_systematic_rmse(df, x, y)$.estimate
  )

  expect_equal(
    ww_unsystematic_rmse_vec(x, y),
    ww_unsystematic_rmse(df, x, y)$.estimate
  )
})
