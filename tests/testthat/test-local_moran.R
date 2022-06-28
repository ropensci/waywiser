test_that("Local Moran statistics are stable", {

  data(guerry_nb, package = "sfdep")

  guerry_modeled <- guerry_nb
  guerry_lm <- lm(crime_pers ~ literacy, guerry_modeled)
  guerry_modeled$predictions <- predict(guerry_lm, guerry_modeled)

  ctg <- guerry_modeled$nb
  wts <- guerry_modeled$wt

  resid <- guerry_modeled$crime_pers - guerry_modeled$predictions

  set.seed(123)
  expect_snapshot(
    (df_local_i <- ww_local_moran_i(guerry_modeled, crime_pers, predictions, ctg, wts))
  )

  set.seed(123)
  expect_snapshot(
    (df_local_i_p <- ww_local_moran_pvalue(guerry_modeled, crime_pers, predictions, ctg, wts))
  )

  set.seed(123)
  expect_snapshot(
    (df_local_i_both <- ww_local_moran(guerry_modeled, crime_pers, predictions, ctg, wts))
  )

  set.seed(123)
  expect_snapshot(
    (vec_local_i <- ww_local_moran_i_vec(guerry_modeled$crime_pers, guerry_modeled$predictions, ctg, wts))
  )

  set.seed(123)
  expect_snapshot(
    (vec_local_i_p <- ww_local_moran_pvalue_vec(guerry_modeled$crime_pers, guerry_modeled$predictions, ctg, wts))
  )

  expect_identical(
    df_local_i,
    df_local_i_both[df_local_i_both$.metric == "local_moran_i", ]
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
  sfdep_output <- sfdep::local_moran(
    x = resid,
    nb = ctg,
    wt = wts
  )

  expect_identical(
    vec_local_i,
    sfdep_output$ii
  )

})
