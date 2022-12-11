# srr: expected failures for ww_systematic_mpd

    Code
      ww_systematic_mpd_vec(1:5, 1:4)
    Error <rlang_error>
      Length of `truth` (5) and `estimate` (4) must match.

---

    Code
      ww_systematic_mpd_vec(1:4, 1:5)
    Error <rlang_error>
      Length of `truth` (4) and `estimate` (5) must match.

---

    Code
      ww_systematic_mpd(char_df, x, y)
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(truth = x, estimate = y, na_rm = na_rm)`.
      Caused by error in `validate_class()`:
      ! `estimate` should be a numeric but a character was supplied.

---

    Code
      ww_systematic_mpd(char_df, y, x)
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(truth = y, estimate = x, na_rm = na_rm)`.
      Caused by error in `validate_class()`:
      ! `truth` should be a numeric but a character was supplied.

---

    Code
      ww_systematic_mpd_vec(as.character(1:5), 1:4)
    Error <rlang_error>
      `truth` should be a numeric but a character was supplied.

---

    Code
      ww_systematic_mpd_vec(1:5, as.character(1:4))
    Error <rlang_error>
      `estimate` should be a numeric but a character was supplied.

---

    Code
      ww_systematic_mpd(list_df, x, y)
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(truth = x, estimate = y, na_rm = na_rm)`.
      Caused by error in `validate_class()`:
      ! `estimate` should be a numeric but a list was supplied.

---

    Code
      ww_systematic_mpd(list_df, y, x)
    Error <rlang_error>
      Problem while computing `.estimate = metric_fn(truth = y, estimate = x, na_rm = na_rm)`.
      Caused by error in `validate_class()`:
      ! `truth` should be a numeric but a list was supplied.

---

    Code
      ww_systematic_mpd_vec(1:4, 1:4)
    Output
      [1] 0

---

    Code
      ww_systematic_mpd(all_identical, x, y)
    Output
      # A tibble: 1 x 3
        .metric        .estimator .estimate
        <chr>          <chr>          <dbl>
      1 systematic_mpd standard           0

