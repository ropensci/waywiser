test_that("Global Geary statistics are stable", {

  skip_if_not(rlang::is_installed("sfdep"))

  data(guerry, package = "sfdep")

  guerry_modeled <- guerry
  guerry_lm <- lm(crime_pers ~ literacy, guerry_modeled)
  guerry_modeled$predictions <- predict(guerry_lm, guerry_modeled)

  weights <- ww_build_weights(guerry)

  resid <- guerry_modeled$crime_pers - guerry_modeled$predictions

  expect_snapshot(
    {
      df_global_c <- ww_global_geary_c(guerry_modeled, crime_pers, predictions)
      df_global_c[1:3]
    }
  )

  expect_snapshot(
    {
      df_global_c_p <- ww_global_geary_pvalue(guerry_modeled, crime_pers, predictions)
      df_global_c_p[1:3]
    }
  )

  expect_snapshot(
    {
      df_global_c_both <- ww_global_geary(guerry_modeled, crime_pers, predictions)
      df_global_c_both[1:3]
    }
  )

  expect_snapshot(
    (vec_global_c <- ww_global_geary_c_vec(guerry_modeled$crime_pers, guerry_modeled$predictions, weights))
  )

  expect_snapshot(
    (vec_global_c_p <- ww_global_geary_pvalue_vec(guerry_modeled$crime_pers, guerry_modeled$predictions, weights))
  )

  expect_identical(
    df_global_c,
    df_global_c_both[df_global_c_both$.metric == "global_geary_c", ]
  )

  expect_identical(
    df_global_c_p,
    df_global_c_both[df_global_c_both$.metric == "global_geary_pvalue", ]
  )

  expect_identical(
    df_global_c$.estimate,
    vec_global_c
  )

  expect_identical(
    df_global_c_p$.estimate,
    vec_global_c_p
  )

  set.seed(123)
  spdep_output <- spdep::geary.test(resid, weights)

  expect_identical(
    vec_global_c,
    spdep_output$estimate[[1]]
  )

  expect_identical(
    vec_global_c_p,
    spdep_output$p.value
  )

})
