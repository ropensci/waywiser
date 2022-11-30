set.seed(123)
test_that("Local Getis-Ord statistics are stable", {

 

 

  guerry_modeled <- guerry
  guerry_lm <- lm(Crm_prs ~ Litercy, guerry_modeled)
  guerry_modeled$predictions <- predict(guerry_lm, guerry_modeled)

  weights <- ww_build_weights(guerry)

  resid <- guerry_modeled$Crm_prs - guerry_modeled$predictions

  expect_snapshot(
    {
      df_local_i <- ww_local_getis_ord_g(guerry_modeled, Crm_prs, predictions)
      df_local_i[1:3]
    }
  )

  set.seed(123)
  expect_snapshot(
    {
      df_local_i_p <- ww_local_getis_ord_pvalue(guerry_modeled, Crm_prs, predictions)
      df_local_i_p[1:3]
    }
  )

  set.seed(123)
  expect_snapshot(
    {
      df_local_i_both <- ww_local_getis_ord(guerry_modeled, Crm_prs, predictions)
      df_local_i_both[1:3]
    }
  )

  expect_snapshot(
    (vec_local_i <- ww_local_getis_ord_g_vec(guerry_modeled$Crm_prs, guerry_modeled$predictions, weights))
  )

  set.seed(123)
  expect_snapshot(
    (vec_local_i_p <- ww_local_getis_ord_pvalue_vec(guerry_modeled$Crm_prs, guerry_modeled$predictions, weights))
  )

  expect_identical(
    df_local_i,
    df_local_i_both[df_local_i_both$.metric == "local_getis_ord_g", ]
  )

  expect_identical(
    df_local_i_p,
    df_local_i_both[df_local_i_both$.metric == "local_getis_ord_g_pvalue", ]
  )

  expect_identical(
    df_local_i$.estimate,
    vec_local_i
  )

  expect_identical(
    df_local_i_p$.estimate,
    vec_local_i_p
  )

  expect_identical(
    vec_local_i,
    as.vector(spdep::localG(resid, weights))
  )

  set.seed(123)
  spdep_output <- spdep::localG_perm(resid, weights)

  expect_identical(
    vec_local_i_p,
    as.vector(attr(spdep_output, "internals")[, 4])
  )

})