# srr: expected failures for ww_willmott_d

    Code
      ww_willmott_d_vec(1:5, 1:4)
    Error <rlang_error>
      Length of `truth` (5) and `estimate` (4) must match.

---

    Code
      ww_willmott_d_vec(1:4, 1:5)
    Error <rlang_error>
      Length of `truth` (4) and `estimate` (5) must match.

---

    Code
      ww_willmott_d(char_df, x, y)
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(...)`.
      Caused by error in `yardstick_vec()`:
      ! `estimate` must be numeric.

---

    Code
      ww_willmott_d(char_df, y, x)
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(...)`.
      Caused by error in `yardstick_vec()`:
      ! `truth` must be numeric.

---

    Code
      ww_willmott_d_vec(as.character(1:5), 1:4)
    Error <rlang_error>
      `truth` must be numeric.

---

    Code
      ww_willmott_d_vec(1:5, as.character(1:4))
    Error <rlang_error>
      `estimate` must be numeric.

---

    Code
      ww_willmott_d(list_df, x, y)
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(...)`.
      Caused by error in `yardstick_vec()`:
      ! `estimate` must be numeric.

---

    Code
      ww_willmott_d(list_df, y, x)
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(...)`.
      Caused by error in `yardstick_vec()`:
      ! `truth` must be numeric.

---

    Code
      ww_willmott_d(missing_df, x, y)$.estimate
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(...)`.
      Caused by error in `yardstick_vec()`:
      ! Missing values in true values (`truth`).
      i Either process your data to fix the NA values or set `na_action`.

---

    Code
      ww_willmott_d(missing_df, y, x)$.estimate
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(...)`.
      Caused by error in `yardstick_vec()`:
      ! Missing values in true values (`truth`).
      i Either process your data to fix the NA values or set `na_action`.

---

    Code
      ww_willmott_d_vec(missing_df$y, missing_df$x)
    Error <rlang_error>
      Missing values in true values (`truth`).
      i Either process your data to fix the NA values or set `na_action`.

---

    Code
      ww_willmott_d_vec(missing_df$x, missing_df$y)
    Error <rlang_error>
      Missing values in true values (`truth`).
      i Either process your data to fix the NA values or set `na_action`.

---

    Code
      ww_willmott_d_vec(numeric(), numeric())
    Error <rlang_error>
      0 values were passed to `truth`.

---

    Code
      ww_willmott_d(empty_df, x, y)
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(...)`.
      Caused by error in `yardstick_vec()`:
      ! 0 values were passed to `truth`.

---

    Code
      ww_willmott_d(empty_df, y, x)
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(...)`.
      Caused by error in `yardstick_vec()`:
      ! 0 values were passed to `truth`.

---

    Code
      ww_willmott_d_vec(rep(NA_real_, 4), 4:1)
    Error <rlang_error>
      Missing values in true values (`truth`).
      i Either process your data to fix the NA values or set `na_action`.

---

    Code
      ww_willmott_d_vec(1:4, rep(NA_real_, 4))
    Error <rlang_error>
      Missing values in estimated values (`estimate`).
      i Either process your data to fix the NA values or set `na_action`.

---

    Code
      ww_willmott_d(all_na, x, y)
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(...)`.
      Caused by error in `yardstick_vec()`:
      ! Missing values in true values (`truth`).
      i Either process your data to fix the NA values or set `na_action`.

---

    Code
      ww_willmott_d(all_na, y, x)
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(...)`.
      Caused by error in `yardstick_vec()`:
      ! Missing values in estimated values (`estimate`).
      i Either process your data to fix the NA values or set `na_action`.

---

    Code
      ww_willmott_d_vec(1:4, 1:4)
    Output
      [1] 1

---

    Code
      ww_willmott_d(all_identical, x, y)
    Output
      # A tibble: 1 x 3
        .metric    .estimator .estimate
        <chr>      <chr>          <dbl>
      1 willmott_d standard           1

