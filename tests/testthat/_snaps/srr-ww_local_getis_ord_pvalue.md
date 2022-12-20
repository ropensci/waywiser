# srr: expected failures for ww_local_getis_ord_pvalue

    Code
      ww_local_getis_ord_pvalue_vec(worldclim_predicted$response, tail(
        worldclim_predicted$predicted, -1), worldclim_weights)
    Error <rlang_error>
      Length of `truth` (10000) and `estimate` (9999) must match.

---

    Code
      ww_local_getis_ord_pvalue_vec(tail(worldclim_predicted$response, -1),
      worldclim_predicted$predicted, worldclim_weights)
    Error <rlang_error>
      Length of `truth` (9999) and `estimate` (10000) must match.

---

    Code
      ww_local_getis_ord_pvalue(worldclim_predicted, predicted, response)
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(...)`.
      Caused by error in `validate_class()`:
      ! `truth` should be a numeric but a character was supplied.

---

    Code
      ww_local_getis_ord_pvalue(worldclim_predicted, response, predicted)
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(...)`.
      Caused by error in `validate_class()`:
      ! `estimate` should be a numeric but a character was supplied.

---

    Code
      ww_local_getis_ord_pvalue_vec(worldclim_predicted$response, worldclim_predicted$
        predicted, worldclim_weights)
    Error <rlang_error>
      `estimate` should be a numeric but a character was supplied.

---

    Code
      ww_local_getis_ord_pvalue_vec(worldclim_predicted$predicted,
      worldclim_predicted$response, worldclim_weights)
    Error <rlang_error>
      `truth` should be a numeric but a character was supplied.

---

    Code
      ww_local_getis_ord_pvalue(worldclim_predicted, response, predicted)
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(...)`.
      Caused by error in `validate_class()`:
      ! `estimate` should be a numeric but a list was supplied.

---

    Code
      ww_local_getis_ord_pvalue(worldclim_predicted, predicted, response)
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(...)`.
      Caused by error in `validate_class()`:
      ! `truth` should be a numeric but a list was supplied.

