test_that("Local geary statistics are stable", {

  skip_if_not(rlang::is_installed("sfdep"))

  data(guerry, package = "sfdep")

  guerry_modeled <- guerry
  guerry_lm <- lm(crime_pers ~ literacy, guerry_modeled)
  guerry_modeled$predictions <- predict(guerry_lm, guerry_modeled)

  weights <- ww_build_weights(guerry)

  resid <- guerry_modeled$crime_pers - guerry_modeled$predictions

  expect_snapshot(
    {
      df_local_c <- ww_local_geary_c(guerry_modeled, crime_pers, predictions)
      df_local_c[1:3]
    }
  )

  set.seed(123)
  expect_snapshot(
    {
      df_local_c_p <- ww_local_geary_pvalue(guerry_modeled, crime_pers, predictions)
      df_local_c_p[1:3]
    }
  )

  set.seed(123)
  expect_snapshot(
    {
      df_local_c_both <- ww_local_geary(guerry_modeled, crime_pers, predictions)
      df_local_c_both[1:3]
    }
  )

  expect_snapshot(
    (vec_local_c <- ww_local_geary_c_vec(guerry_modeled$crime_pers, guerry_modeled$predictions, weights))
  )

  set.seed(123)
  expect_snapshot(
    (vec_local_c_p <- ww_local_geary_pvalue_vec(guerry_modeled$crime_pers, guerry_modeled$predictions, weights))
  )

  expect_identical(
    df_local_c,
    df_local_c_both[df_local_c_both$.metric == "local_geary_c", ]
  )

  expect_identical(
    df_local_c_p,
    df_local_c_both[df_local_c_both$.metric == "local_geary_pvalue", ]
  )

  expect_identical(
    df_local_c$.estimate,
    vec_local_c
  )

  expect_identical(
    df_local_c_p$.estimate,
    vec_local_c_p
  )

  set.seed(123)
  spdep_output <- spdep::localC_perm(resid, weights)

  expect_identical(
    vec_local_c,
    as.vector(spdep_output)
  )

  expect_identical(
    vec_local_c_p,
    as.vector(attr(spdep_output, "pseudo-p")[, 4])
  )

})
