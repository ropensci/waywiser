# srr: expected failures for ww_systematic_mse

    Code
      ww_systematic_mse_vec(1:5, 1:4)
    Error <rlang_error>
      Length of `truth` (5) and `estimate` (4) must match.

---

    Code
      ww_systematic_mse_vec(1:4, 1:5)
    Error <rlang_error>
      Length of `truth` (4) and `estimate` (5) must match.

---

    Code
      ww_systematic_mse(char_df, x, y)
    Error <rlang_error>
      i In argument: `.estimate = fn(truth = .data[["x"]], estimate = .data[["y"]], na_rm = na_rm)`.
      Caused by error in `yardstick_vec()`:
      ! `estimate` must be numeric.

---

    Code
      ww_systematic_mse(char_df, y, x)
    Error <rlang_error>
      i In argument: `.estimate = fn(truth = .data[["y"]], estimate = .data[["x"]], na_rm = na_rm)`.
      Caused by error in `yardstick_vec()`:
      ! `truth` must be numeric.

---

    Code
      ww_systematic_mse_vec(as.character(1:5), 1:4)
    Error <rlang_error>
      `truth` must be numeric.

---

    Code
      ww_systematic_mse_vec(1:5, as.character(1:4))
    Error <rlang_error>
      `estimate` must be numeric.

---

    Code
      ww_systematic_mse(list_df, x, y)
    Error <rlang_error>
      i In argument: `.estimate = fn(truth = .data[["x"]], estimate = .data[["y"]], na_rm = na_rm)`.
      Caused by error in `yardstick_vec()`:
      ! `estimate` must be numeric.

---

    Code
      ww_systematic_mse(list_df, y, x)
    Error <rlang_error>
      i In argument: `.estimate = fn(truth = .data[["y"]], estimate = .data[["x"]], na_rm = na_rm)`.
      Caused by error in `yardstick_vec()`:
      ! `truth` must be numeric.

---

    Code
      ww_systematic_mse(missing_df, x, y)$.estimate
    Output
      [1] 6.573841e-32

---

    Code
      ww_systematic_mse(missing_df, y, x)$.estimate
    Output
      [1] 6.573841e-32

---

    Code
      ww_systematic_mse_vec(missing_df$y, missing_df$x)
    Output
      [1] 6.573841e-32

---

    Code
      ww_systematic_mse_vec(missing_df$x, missing_df$y)
    Output
      [1] 6.573841e-32

---

    Code
      ww_systematic_mse_vec(numeric(), numeric())
    Error <rlang_error>
      0 non-missing values were passed to `truth`.

---

    Code
      ww_systematic_mse(empty_df, x, y)
    Error <rlang_error>
      i In argument: `.estimate = fn(truth = .data[["x"]], estimate = .data[["y"]], na_rm = na_rm)`.
      Caused by error in `yardstick_vec()`:
      ! 0 non-missing values were passed to `truth`.

---

    Code
      ww_systematic_mse(empty_df, y, x)
    Error <rlang_error>
      i In argument: `.estimate = fn(truth = .data[["y"]], estimate = .data[["x"]], na_rm = na_rm)`.
      Caused by error in `yardstick_vec()`:
      ! 0 non-missing values were passed to `truth`.

---

    Code
      ww_systematic_mse_vec(rep(NA_real_, 4), 4:1)
    Error <rlang_error>
      0 non-missing values were passed to `truth`.

---

    Code
      ww_systematic_mse_vec(1:4, rep(NA_real_, 4))
    Error <rlang_error>
      0 non-missing values were passed to `truth`.

---

    Code
      ww_systematic_mse(all_na, x, y)
    Error <rlang_error>
      i In argument: `.estimate = fn(truth = .data[["x"]], estimate = .data[["y"]], na_rm = na_rm)`.
      Caused by error in `yardstick_vec()`:
      ! 0 non-missing values were passed to `truth`.

---

    Code
      ww_systematic_mse(all_na, y, x)
    Error <rlang_error>
      i In argument: `.estimate = fn(truth = .data[["y"]], estimate = .data[["x"]], na_rm = na_rm)`.
      Caused by error in `yardstick_vec()`:
      ! 0 non-missing values were passed to `truth`.

---

    Code
      ww_systematic_mse_vec(1:4, 1:4)
    Output
      [1] 0

---

    Code
      ww_systematic_mse(all_identical, x, y)
    Output
      # A tibble: 1 x 3
        .metric        .estimator .estimate
        <chr>          <chr>          <dbl>
      1 systematic_mse standard           0
