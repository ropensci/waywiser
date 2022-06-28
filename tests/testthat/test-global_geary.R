test_that("Global Geary statistics are stable", {

  data(guerry_nb, package = "sfdep")

  guerry_modeled <- guerry_nb
  guerry_lm <- lm(crime_pers ~ literacy, guerry_modeled)
  guerry_modeled$predictions <- predict(guerry_lm, guerry_modeled)

  ctg <- guerry_modeled$nb
  wts <- guerry_modeled$wt

  resid <- guerry_modeled$crime_pers - guerry_modeled$predictions

  set.seed(123)
  expect_snapshot(
    {
      df_global_c <- ww_global_geary_c(guerry_modeled, crime_pers, predictions, ctg, wts)
      df_global_c[1:3]
    }
  )

  set.seed(123)
  expect_snapshot(
    {
      df_global_c_p <- ww_global_geary_pvalue(guerry_modeled, crime_pers, predictions, ctg, wts)
      df_global_c_p[1:3]
    }
  )

  set.seed(123)
  expect_snapshot(
    {
      df_global_c_both <- ww_global_geary(guerry_modeled, crime_pers, predictions, ctg, wts)
      df_global_c_both[1:3]
    }
  )

  set.seed(123)
  expect_snapshot(
    (vec_global_c <- ww_global_geary_c_vec(guerry_modeled$crime_pers, guerry_modeled$predictions, ctg, wts))
  )

  set.seed(123)
  expect_snapshot(
    (vec_global_c_p <- ww_global_geary_pvalue_vec(guerry_modeled$crime_pers, guerry_modeled$predictions, ctg, wts))
  )

  expect_identical(
    df_global_c,
    df_global_c_both[df_global_c_both$.metric == "global_geary_c", ]
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
  sfdep_output <- sfdep::global_c_perm(
    x = resid,
    nb = ctg,
    wt = wts
  )

  expect_identical(
    vec_global_c,
    as.vector(sfdep_output$statistic)
  )

})
