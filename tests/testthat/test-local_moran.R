test_that("Local Moran statistics are stable", {

  skip_if_not(rlang::is_installed("sfdep"))

  data(guerry, package = "sfdep")

  guerry_modeled <- guerry
  guerry_lm <- lm(crime_pers ~ literacy, guerry_modeled)
  guerry_modeled$predictions <- predict(guerry_lm, guerry_modeled)

  weights <- ww_build_weights(guerry)

  resid <- guerry_modeled$crime_pers - guerry_modeled$predictions

  expect_snapshot(
    {
      df_local_i <- ww_local_moran_i(guerry_modeled, crime_pers, predictions)
      df_local_i[1:3]
    }
  )

  expect_snapshot(
    {
      df_local_i_p <- ww_local_moran_pvalue(guerry_modeled, crime_pers, predictions)
      df_local_i_p[1:3]
    }
  )

  expect_snapshot(
    {
      df_local_i_both <- ww_local_moran(guerry_modeled, crime_pers, predictions)
      df_local_i_both[1:3]
    }
  )

  expect_snapshot(
    (vec_local_i <- ww_local_moran_i_vec(guerry_modeled$crime_pers, guerry_modeled$predictions, weights))
  )

  expect_snapshot(
    (vec_local_i_p <- ww_local_moran_pvalue_vec(guerry_modeled$crime_pers, guerry_modeled$predictions, weights))
  )

  expect_identical(
    df_local_i,
    df_local_i_both[df_local_i_both$.metric == "local_moran_i", ]
  )

  expect_identical(
    df_local_i_p,
    df_local_i_both[df_local_i_both$.metric == "local_moran_pvalue", ]
  )

  expect_identical(
    df_local_i$.estimate,
    vec_local_i
  )

  expect_identical(
    df_local_i_p$.estimate,
    vec_local_i_p
  )

  set.seed(123)
  spdep_output <- spdep::localmoran(resid, weights)

  expect_identical(
    vec_local_i,
    spdep_output[, 1]
  )

  expect_identical(
    vec_local_i_p,
    spdep_output[, 5]
  )

})
