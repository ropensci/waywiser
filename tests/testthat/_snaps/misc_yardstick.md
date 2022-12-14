# passing functions to build weights

    Code
      df_local_i <- ww_local_getis_ord_g(guerry_modeled, Crm_prs, predictions, wt = ww_build_weights)
      df_local_i[1:3]
    Output
      # A tibble: 85 x 3
         .metric           .estimator .estimate
         <chr>             <chr>          <dbl>
       1 local_getis_ord_g standard       0.913
       2 local_getis_ord_g standard       2.49 
       3 local_getis_ord_g standard       2.15 
       4 local_getis_ord_g standard      -1.58 
       5 local_getis_ord_g standard      -1.19 
       6 local_getis_ord_g standard      -1.68 
       7 local_getis_ord_g standard       0.627
       8 local_getis_ord_g standard      -1.60 
       9 local_getis_ord_g standard       0.964
      10 local_getis_ord_g standard      -2.71 
      # ... with 75 more rows

# edge cases

    Code
      ww_local_getis_ord_g(guerry_modeled, Crm_prs, predictions, wt = list())
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(...)`.
      Caused by error in `spatial_yardstick_vec()`:
      ! `wt` must be a 'listw' object
      i You can create 'listw' objects using `build_weights()`

---

    Code
      ww_local_getis_ord_g_vec(as.character(crm), prd, structure(list(), class = "listw"))
    Error <rlang_error>
      `truth` must be numeric.

---

    Code
      ww_local_getis_ord_g_vec(crm, as.character(prd), structure(list(), class = "listw"))
    Error <rlang_error>
      `estimate` must be numeric.

---

    Code
      ww_local_getis_ord_g_vec(as.matrix(crm), prd, structure(list(), class = "listw"))
    Error <rlang_error>
      `truth` must be a numeric vector.

---

    Code
      ww_local_getis_ord_g_vec(crm, as.matrix(prd), structure(list(), class = "listw"))
    Error <rlang_error>
      `estimate` must be a numeric vector.

---

    Code
      ww_local_getis_ord_g_vec(crm, numeric(), structure(list(), class = "listw"))
    Error <rlang_error>
      0 values were passed to `estimate`.

---

    Code
      ww_local_getis_ord_g_vec(crm, prd, structure(list(), class = "listw"),
      na_action = na.omit)
    Error <rlang_error>
      `truth` and `estimate` were not the same length as `wt$neighbours` after running `na_action`.

---

    Code
      withr::with_seed(123, ww_local_getis_ord_g_vec(crm, prd, structure(list(),
      class = "listw"), na_action = function(x) runif(sample(1:100, sample(1:100, 1)))))
    Error <rlang_error>
      `truth` and `estimate` were not the same length after running `na_action`.

