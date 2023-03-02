# srr: expected failures for ww_unsystematic_rmpd

    Code
      ww_unsystematic_rmpd_vec(1:5, 1:4)
    Error <rlang_error>
      Length of `truth` (5) and `estimate` (4) must match.

---

    Code
      ww_unsystematic_rmpd_vec(1:4, 1:5)
    Error <rlang_error>
      Length of `truth` (4) and `estimate` (5) must match.

---

    Code
      ww_unsystematic_rmpd(char_df, x, y)
    Error <rlang_error>
      i In argument: `.estimate = fn(truth = .data[["x"]], estimate = .data[["y"]], na_rm = .env[["na_rm"]])`.
      Caused by error in `yardstick_vec()`:
      ! `estimate` must be numeric.

---

    Code
      ww_unsystematic_rmpd(char_df, y, x)
    Error <rlang_error>
      i In argument: `.estimate = fn(truth = .data[["y"]], estimate = .data[["x"]], na_rm = .env[["na_rm"]])`.
      Caused by error in `yardstick_vec()`:
      ! `truth` must be numeric.

---

    Code
      ww_unsystematic_rmpd_vec(as.character(1:5), 1:4)
    Error <rlang_error>
      `truth` must be numeric.

---

    Code
      ww_unsystematic_rmpd_vec(1:5, as.character(1:4))
    Error <rlang_error>
      `estimate` must be numeric.

---

    Code
      ww_unsystematic_rmpd(list_df, x, y)
    Error <rlang_error>
      i In argument: `.estimate = fn(truth = .data[["x"]], estimate = .data[["y"]], na_rm = .env[["na_rm"]])`.
      Caused by error in `yardstick_vec()`:
      ! `estimate` must be numeric.

---

    Code
      ww_unsystematic_rmpd(list_df, y, x)
    Error <rlang_error>
      i In argument: `.estimate = fn(truth = .data[["y"]], estimate = .data[["x"]], na_rm = .env[["na_rm"]])`.
      Caused by error in `yardstick_vec()`:
      ! `truth` must be numeric.

---

    Code
      round(ww_unsystematic_rmpd(missing_df, x, y)$.estimate, 15)
    Output
      [1] 0

---

    Code
      round(ww_unsystematic_rmpd(missing_df, y, x)$.estimate, 15)
    Output
      [1] 0

---

    Code
      round(ww_unsystematic_rmpd_vec(missing_df$y, missing_df$x), 15)
    Output
      [1] 0

---

    Code
      round(ww_unsystematic_rmpd_vec(missing_df$x, missing_df$y), 15)
    Output
      [1] 0

---

    Code
      ww_unsystematic_rmpd_vec(numeric(), numeric())
    Error <rlang_error>
      0 non-missing values were passed to `truth`.

---

    Code
      ww_unsystematic_rmpd(empty_df, x, y)
    Error <rlang_error>
      i In argument: `.estimate = fn(truth = .data[["x"]], estimate = .data[["y"]], na_rm = .env[["na_rm"]])`.
      Caused by error in `yardstick_vec()`:
      ! 0 non-missing values were passed to `truth`.

---

    Code
      ww_unsystematic_rmpd(empty_df, y, x)
    Error <rlang_error>
      i In argument: `.estimate = fn(truth = .data[["y"]], estimate = .data[["x"]], na_rm = .env[["na_rm"]])`.
      Caused by error in `yardstick_vec()`:
      ! 0 non-missing values were passed to `truth`.

---

    Code
      ww_unsystematic_rmpd_vec(rep(NA_real_, 4), 4:1)
    Error <rlang_error>
      0 non-missing values were passed to `truth`.

---

    Code
      ww_unsystematic_rmpd_vec(1:4, rep(NA_real_, 4))
    Error <rlang_error>
      0 non-missing values were passed to `truth`.

---

    Code
      ww_unsystematic_rmpd(all_na, x, y)
    Error <rlang_error>
      i In argument: `.estimate = fn(truth = .data[["x"]], estimate = .data[["y"]], na_rm = .env[["na_rm"]])`.
      Caused by error in `yardstick_vec()`:
      ! 0 non-missing values were passed to `truth`.

---

    Code
      ww_unsystematic_rmpd(all_na, y, x)
    Error <rlang_error>
      i In argument: `.estimate = fn(truth = .data[["y"]], estimate = .data[["x"]], na_rm = .env[["na_rm"]])`.
      Caused by error in `yardstick_vec()`:
      ! 0 non-missing values were passed to `truth`.

---

    Code
      ww_unsystematic_rmpd_vec(1:4, 1:4)
    Output
      [1] 0

---

    Code
      ww_unsystematic_rmpd(all_identical, x, y)
    Output
      # A tibble: 1 x 3
        .metric           .estimator .estimate
        <chr>             <chr>          <dbl>
      1 unsystematic_rmpd standard           0

