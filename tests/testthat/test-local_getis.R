test_that("Getis-Ord statistics are stable", {

  data(guerry, package = "sfdep")

  guerry_modeled <- guerry
  guerry_lm <- lm(crime_pers ~ literacy, guerry_modeled)
  guerry_modeled$predictions <- predict(guerry_lm, guerry_modeled)

  ctg <- st_contiguity(guerry)
  wts <- st_weights(ctg)

  resid <- guerry_modeled$crime_pers - guerry_modeled$predictions

  set.seed(123)
  expect_snapshot(
    (df_local_g <- ww_local_getis_ord_g(guerry_modeled, crime_pers, predictions, ctg, wts))
  )

  set.seed(123)
  expect_snapshot(
    (df_local_g_p <- ww_local_getis_ord_g_pvalue(guerry_modeled, crime_pers, predictions, ctg, wts))
  )

  set.seed(123)
  expect_snapshot(
    (df_local_g_both <- ww_local_g(guerry_modeled, crime_pers, predictions, ctg, wts))
  )

  set.seed(123)
  expect_snapshot(
    (df_local_gstar <- ww_local_getis_ord_g_star(guerry_modeled, crime_pers, predictions, ctg, wts))
  )

  set.seed(123)
  expect_snapshot(
    (df_local_gstar_p <- ww_local_getis_ord_g_star_pvalue(guerry_modeled, crime_pers, predictions, ctg, wts))
  )

  set.seed(123)
  expect_snapshot(
    (df_local_gstar_both <- ww_local_g_star(guerry_modeled, crime_pers, predictions, ctg, wts))
  )

  set.seed(123)
  expect_snapshot(
    (vec_local_g <- ww_local_getis_ord_g_vec(guerry_modeled$crime_pers, guerry_modeled$predictions, ctg, wts))
  )

  set.seed(123)
  expect_snapshot(
    (vec_local_g_p <- ww_local_getis_ord_g_pvalue_vec(guerry_modeled$crime_pers, guerry_modeled$predictions, ctg, wts))
  )

  set.seed(123)
  expect_snapshot(
    (vec_local_gstar <- ww_local_getis_ord_g_star_vec(guerry_modeled$crime_pers, guerry_modeled$predictions, ctg, wts))
  )

  set.seed(123)
  expect_snapshot(
    (vec_local_gstar_p <- ww_local_getis_ord_g_star_pvalue_vec(guerry_modeled$crime_pers, guerry_modeled$predictions, ctg, wts))
  )

  expect_identical(
    df_local_g,
    df_local_g_both[df_local_g_both$.metric == "local_getis_ord_g", ]
  )

  expect_identical(
    df_local_g$.estimate,
    vec_local_g
  )

  expect_identical(
    df_local_g_p$.estimate,
    vec_local_g_p
  )

  expect_identical(
    df_local_gstar,
    df_local_gstar_both[df_local_gstar_both$.metric == "local_getis_ord_g_star", ]
  )

  expect_identical(
    df_local_gstar$.estimate,
    vec_local_gstar
  )

  expect_identical(
    df_local_gstar_p$.estimate,
    vec_local_gstar_p
  )

  set.seed(123)
  sfdep_output <- sfdep::local_g_perm(
    x = resid,
    nb = ctg,
    wt = wts
  )

  expect_identical(
    vec_local_g,
    sfdep_output$gi
  )

  expect_identical(
    vec_local_g_p,
    sfdep_output$p_value
  )

  set.seed(123)
  sfdep_output <- sfdep::local_gstar_perm(
    x = resid,
    nb = ctg,
    wt = wts
  )

  expect_identical(
    vec_local_gstar,
    sfdep_output$gi
  )

  expect_identical(
    vec_local_gstar_p,
    sfdep_output$p_value
  )

})
