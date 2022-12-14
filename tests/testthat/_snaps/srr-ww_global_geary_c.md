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
      Problem while computing `.estimate = metric_fn(...)`.
      Caused by error in `yardstick_vec()`:
      ! `truth` must be numeric.

---

    Code
      ww_global_geary_c(worldclim_predicted, response, predicted)
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(...)`.
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
      Problem while computing `.estimate = metric_fn(...)`.
      Caused by error in `yardstick_vec()`:
      ! `estimate` must be numeric.

---

    Code
      ww_global_geary_c(worldclim_predicted, predicted, response)
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(...)`.
      Caused by error in `yardstick_vec()`:
      ! `truth` must be numeric.

---

    Code
      ww_global_geary_c(worldclim_predicted, predicted, response)$.estimate
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(...)`.
      Caused by error in `yardstick_vec()`:
      ! Missing values in estimated values (`estimate`).
      i Either process your data to fix the NA values or set `na_action`.

---

    Code
      ww_global_geary_c(worldclim_predicted, response, predicted)$.estimate
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(...)`.
      Caused by error in `yardstick_vec()`:
      ! Missing values in true values (`truth`).
      i Either process your data to fix the NA values or set `na_action`.

---

    Code
      ww_global_geary_c_vec(worldclim_predicted$predicted, worldclim_predicted$
        response, worldclim_weights)
    Error <rlang_error>
      Missing values in estimated values (`estimate`).
      i Either process your data to fix the NA values or set `na_action`.

---

    Code
      ww_global_geary_c_vec(worldclim_predicted$response, worldclim_predicted$
        predicted, worldclim_weights)
    Error <rlang_error>
      Missing values in true values (`truth`).
      i Either process your data to fix the NA values or set `na_action`.

---

    Code
      ww_global_geary_c_vec(numeric(), numeric(), structure(list(), class = "listw"))
    Error <rlang_error>
      0 values were passed to `truth`.

---

    Code
      ww_global_geary_c(head(worldclim_predicted, 0), response, predicted, structure(
        list(), class = "listw"))
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(...)`.
      Caused by error in `yardstick_vec()`:
      ! 0 values were passed to `truth`.

---

    Code
      ww_global_geary_c(head(worldclim_predicted, 0), predicted, response, structure(
        list(), class = "listw"))
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(...)`.
      Caused by error in `yardstick_vec()`:
      ! 0 values were passed to `truth`.

---

    Code
      ww_global_geary_c_vec(NA_real_, NA_real_, structure(list(neighbours = 1),
      class = "listw"))
    Error <rlang_error>
      Missing values in true values (`truth`).
      i Either process your data to fix the NA values or set `na_action`.

---

    Code
      ww_global_geary_c(worldclim_predicted, response, predicted)$.estimate
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(...)`.
      Caused by error in `yardstick_vec()`:
      ! Missing values in true values (`truth`).
      i Either process your data to fix the NA values or set `na_action`.

---

    Code
      ww_global_geary_c(worldclim_predicted, predicted, response)$.estimate
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(...)`.
      Caused by error in `yardstick_vec()`:
      ! Missing values in estimated values (`estimate`).
      i Either process your data to fix the NA values or set `na_action`.

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
      # A tibble: 1 x 4
        .metric        .estimator .estimate                                   geometry
        <chr>          <chr>          <dbl>                           <MULTIPOINT [°]>
      1 global_geary_c standard         NaN ((-41.18256 -22.75207), (-42.91933 -23.21~

