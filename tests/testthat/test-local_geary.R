test_that("Local Geary statistics are stable", {

  data(guerry, package = "sfdep")

  guerry_modeled <- guerry
  guerry_lm <- lm(crime_pers ~ literacy, guerry_modeled)
  guerry_modeled$predictions <- predict(guerry_lm, guerry_modeled)

  ctg <- st_contiguity(guerry)
  wts <- st_weights(ctg)

  resid <- guerry_modeled$crime_pers - guerry_modeled$predictions

  set.seed(123)
  expect_snapshot(
    (df_local_c <- ww_local_geary_c(guerry_modeled, crime_pers, predictions, ctg, wts))
  )

  set.seed(123)
  expect_snapshot(
    (df_local_c_p <- ww_local_geary_pvalue(guerry_modeled, crime_pers, predictions, ctg, wts))
  )

  set.seed(123)
  expect_snapshot(
    (df_local_c_both <- ww_local_geary(guerry_modeled, crime_pers, predictions, ctg, wts))
  )

  set.seed(123)
  expect_snapshot(
    (vec_local_c <- ww_local_geary_c_vec(guerry_modeled$crime_pers, guerry_modeled$predictions, ctg, wts))
  )

  set.seed(123)
  expect_snapshot(
    (vec_local_c_p <- ww_local_geary_pvalue_vec(guerry_modeled$crime_pers, guerry_modeled$predictions, ctg, wts))
  )

  expect_identical(
    df_local_c,
    df_local_c_both[df_local_c_both$.metric == "local_geary_c", ]
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
  sfdep_output <- sfdep::local_c_perm(
    x = resid,
    nb = ctg,
    wt = wts
  )

  expect_identical(
    vec_local_c,
    sfdep_output$ci
  )

})
