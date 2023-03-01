# srr: expected failures for ww_global_geary_c

    Code
      ww_global_geary_c_vec(worldclim_predicted$response, tail(worldclim_predicted$
        predicted, -1), worldclim_weights)
    Error <rlang_error>
      Length of `truth` (10000) and `estimate` (9999) must match.

---

    Code
      ww_global_geary_c_vec(tail(worldclim_predicted$response, -1),
      worldclim_predicted$predicted, worldclim_weights)
    Error <rlang_error>
      Length of `truth` (9999) and `estimate` (10000) must match.

---

    Code
      ww_global_geary_c(worldclim_predicted, predicted, response)
    Error <rlang_error>
      i In argument: `.estimate = fn(...)`.
      Caused by error in `yardstick_vec()`:
      ! `truth` must be numeric.

---

    Code
      ww_global_geary_c(worldclim_predicted, response, predicted)
    Error <rlang_error>
      i In argument: `.estimate = fn(...)`.
      Caused by error in `yardstick_vec()`:
      ! `estimate` must be numeric.

---

    Code
      ww_global_geary_c_vec(worldclim_predicted$response, worldclim_predicted$
        predicted, worldclim_weights)
    Error <rlang_error>
      `estimate` must be numeric.

---

    Code
      ww_global_geary_c_vec(worldclim_predicted$predicted, worldclim_predicted$
        response, worldclim_weights)
    Error <rlang_error>
      `truth` must be numeric.

---

    Code
      ww_global_geary_c(worldclim_predicted, response, predicted)
    Error <rlang_error>
      i In argument: `.estimate = fn(...)`.
      Caused by error in `yardstick_vec()`:
      ! `estimate` must be numeric.

---

    Code
      ww_global_geary_c(worldclim_predicted, predicted, response)
    Error <rlang_error>
      i In argument: `.estimate = fn(...)`.
      Caused by error in `yardstick_vec()`:
      ! `truth` must be numeric.

---

    Code
      ww_global_geary_c(worldclim_predicted, predicted, response)$.estimate
    Error <rlang_error>
      Missing values in data.
      i waywiser can't handle missing data for functions that use spatial weights.

---

    Code
      ww_global_geary_c(worldclim_predicted, response, predicted)$.estimate
    Error <rlang_error>
      Missing values in data.
      i waywiser can't handle missing data for functions that use spatial weights.

---

    Code
      ww_global_geary_c_vec(worldclim_predicted$predicted, worldclim_predicted$
        response, worldclim_weights)
    Error <rlang_error>
      Missing values in data.
      i waywiser can't handle missing data for functions that use spatial weights.

---

    Code
      ww_global_geary_c_vec(worldclim_predicted$response, worldclim_predicted$
        predicted, worldclim_weights)
    Error <rlang_error>
      Missing values in data.
      i waywiser can't handle missing data for functions that use spatial weights.

---

    Code
      ww_global_geary_c_vec(numeric(), numeric(), structure(list(), class = "listw"))
    Error <rlang_error>
      0 non-missing values were passed to `truth`.

---

    Code
      ww_global_geary_c(head(worldclim_predicted, 0), response, predicted, structure(
        list(), class = "listw"))
    Error <rlang_error>
      i In argument: `.estimate = fn(...)`.
      Caused by error in `yardstick_vec()`:
      ! 0 non-missing values were passed to `truth`.

---

    Code
      ww_global_geary_c(head(worldclim_predicted, 0), predicted, response, structure(
        list(), class = "listw"))
    Error <rlang_error>
      i In argument: `.estimate = fn(...)`.
      Caused by error in `yardstick_vec()`:
      ! 0 non-missing values were passed to `truth`.

---

    Code
      ww_global_geary_c_vec(NA_real_, NA_real_, structure(list(neighbours = 1),
      class = "listw"))
    Error <rlang_error>
      Missing values in data.
      i waywiser can't handle missing data for functions that use spatial weights.

---

    Code
      ww_global_geary_c(worldclim_predicted, response, predicted)$.estimate
    Error <rlang_error>
      Missing values in data.
      i waywiser can't handle missing data for functions that use spatial weights.

---

    Code
      ww_global_geary_c(worldclim_predicted, predicted, response)$.estimate
    Error <rlang_error>
      Missing values in data.
      i waywiser can't handle missing data for functions that use spatial weights.

---

    Code
      ww_global_geary_c_vec(worldclim_simulation$response, worldclim_simulation$
        response, worldclim_weights)
    Output
      [1] NaN

---

    Code
      ww_global_geary_c(worldclim_simulation, response, response)
    Output
      # A tibble: 1 x 3
        .metric        .estimator .estimate
        <chr>          <chr>          <dbl>
      1 global_geary_c standard         NaN

# other generic srr standards

    Code
      withr::with_seed(123, ww_global_geary_c(worldclim_loaded, bio13, bio19))
    Output
      # A tibble: 1 x 3
        .metric        .estimator .estimate
        <chr>          <chr>          <dbl>
      1 global_geary_c standard      0.0731

---

    Code
      withr::with_seed(123, ww_global_geary_c(worldclim_loaded, bio13, bio19))
    Output
      # A tibble: 1 x 3
        .metric        .estimator .estimate
        <chr>          <chr>          <dbl>
      1 global_geary_c standard      0.0731

---

    Code
      withr::with_seed(123, ww_global_geary_c_vec(worldclim_loaded$bio13,
      worldclim_loaded$bio19, worldclim_weights))
    Output
      [1] 0.07310762

---

    Code
      withr::with_seed(123, ww_global_geary_c_vec(worldclim_loaded$bio13,
      worldclim_loaded$bio19, worldclim_weights))
    Output
      [1] 0.07310762

---

    Code
      withr::with_seed(123, ww_global_geary_c(worldclim_loaded, bio13, bio19))
    Output
      # A tibble: 1 x 3
        .metric        .estimator .estimate
        <chr>          <chr>          <dbl>
      1 global_geary_c standard      0.0731

---

    Code
      withr::with_seed(123, ww_global_geary_c(worldclim_loaded, bio13, bio19,
        function(data) ww_build_weights(ww_make_point_neighbors(data, k = 5))))
    Output
      # A tibble: 1 x 3
        .metric        .estimator .estimate
        <chr>          <chr>          <dbl>
      1 global_geary_c standard       0.155

---

    Code
      withr::with_seed(123, ww_global_geary_c_vec(worldclim_loaded$bio13,
      worldclim_loaded$bio19, other_weights))
    Output
      [1] 0.1546693

---

    Code
      withr::with_seed(123, ww_global_geary_c_vec(worldclim_loaded$bio13,
      worldclim_loaded$bio19, other_weights))
    Output
      [1] 0.1546693

