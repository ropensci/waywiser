test_that("Global Moran statistics are stable", {

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
      df_global_i <- ww_global_moran_i(guerry_modeled, crime_pers, predictions, ctg, wts)
      df_global_i[1:3]
    }
  )

  set.seed(123)
  expect_snapshot(
    {
      df_global_i_p <- ww_global_moran_pvalue(guerry_modeled, crime_pers, predictions, ctg, wts)
      df_global_i_p[1:3]
    }
  )

  set.seed(123)
  expect_snapshot(
    {
      df_global_i_both <- ww_global_moran(guerry_modeled, crime_pers, predictions, ctg, wts)
      df_global_i_both[1:3]
    }
  )

  set.seed(123)
  expect_snapshot(
    (vec_global_i <- ww_global_moran_i_vec(guerry_modeled$crime_pers, guerry_modeled$predictions, ctg, wts))
  )

  set.seed(123)
  expect_snapshot(
    (vec_global_i_p <- ww_global_moran_pvalue_vec(guerry_modeled$crime_pers, guerry_modeled$predictions, ctg, wts))
  )

  expect_identical(
    df_global_i,
    df_global_i_both[df_global_i_both$.metric == "global_moran_i", ]
  )

  expect_identical(
    df_global_i$.estimate,
    vec_global_i
  )

  expect_identical(
    df_global_i_p$.estimate,
    vec_global_i_p
  )

  set.seed(123)
  sfdep_output <- sfdep::global_moran_perm(
    x = resid,
    nb = ctg,
    wt = wts
  )

  expect_identical(
    vec_global_i,
    as.vector(sfdep_output$statistic)
  )

})
