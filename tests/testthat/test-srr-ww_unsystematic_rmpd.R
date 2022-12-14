# This file was generated, do not edit by hand
# Please edit inst/srr_template_nonspatial_yardstick.R instead

test_that("srr: expected failures for ww_unsystematic_rmpd", {
  # Note that this test isn't applicable to data-frame input, which enforces
  # constant column lengths
  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G2.0} Truth and estimate are equal in length:
  expect_snapshot(
    ww_unsystematic_rmpd_vec(1:5, 1:4),
    error = TRUE
  )

  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G2.0} Truth and estimate are equal in length:
  expect_snapshot(
    ww_unsystematic_rmpd_vec(1:4, 1:5),
    error = TRUE
  )

  char_df <- tibble::tibble(x = 1:5, y = letters[1:5])
  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.1} Truth and estimate are numeric:
  expect_snapshot(
    ww_unsystematic_rmpd(char_df, x, y),
    error = TRUE
  )

  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.1} Truth and estimate are numeric:
  expect_snapshot(
    ww_unsystematic_rmpd(char_df, y, x),
    error = TRUE
  )

  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.1} Truth and estimate are numeric:
  expect_snapshot(
    ww_unsystematic_rmpd_vec(as.character(1:5), 1:4),
    error = TRUE
  )

  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.1} Truth and estimate are numeric:
  expect_snapshot(
    ww_unsystematic_rmpd_vec(1:5, as.character(1:4)),
    error = TRUE
  )

  list_df <- tibble::tibble(x = 1:5, y = lapply(1:5, function(x) x))
  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.12} List column inputs fail:
  expect_snapshot(
    ww_unsystematic_rmpd(list_df, x, y),
    error = TRUE
  )

  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.12} List column inputs fail:
  expect_snapshot(
    ww_unsystematic_rmpd(list_df, y, x),
    error = TRUE
  )

  #' @srrstats {G2.13} Missing data is properly handled
  #' @srrstats {G2.15} Missingness is checked
  #' @srrstats {G2.14} Users can specify behavior with NA results
  #' @srrstats {G2.16} NaN is properly handled
  missing_df <- tibble::tibble(x = c(NaN, 2:5), y = c(1:4, NA))
  #' Users can error:
  expect_snapshot(
    ww_unsystematic_rmpd(missing_df, x, y)$.estimate,
    error = TRUE
  )

  #' Users can error:
  expect_snapshot(
    ww_unsystematic_rmpd(missing_df, y, x)$.estimate,
    error = TRUE
  )

  #' Users can error:
  expect_snapshot(
    ww_unsystematic_rmpd_vec(missing_df$y, missing_df$x),
    error = TRUE
  )

  #' Users can error:
  expect_snapshot(
    ww_unsystematic_rmpd_vec(missing_df$x, missing_df$y),
    error = TRUE
  )

  #' @srrstats {G2.14b} Users can ignore NA:
  expect_identical(
    ww_unsystematic_rmpd(missing_df, y, x, na_action = function(x) unlist(na.pass(x)))$.estimate,
    NA_real_
  )

  #' @srrstats {G2.14b} Users can ignore NA:
  expect_identical(
    ww_unsystematic_rmpd(missing_df, x, y, na_action = function(x) unlist(na.pass(x)))$.estimate,
    NA_real_
  )

  #' @srrstats {G2.14b} Users can ignore NA:
  expect_identical(
    ww_unsystematic_rmpd_vec(missing_df$y, missing_df$x, na_action = function(x) unlist(na.pass(x))),
    NA_real_
  )

  #' @srrstats {G2.14b} Users can ignore NA:
  expect_identical(
    ww_unsystematic_rmpd_vec(missing_df$x, missing_df$y, na_action = function(x) unlist(na.pass(x))),
    NA_real_
  )

  #' @srrstats {G2.14b} Users can delete NA values:
  expect_no_error(
    ww_unsystematic_rmpd(missing_df, x, y, na_action = na.omit)
  )

  #' @srrstats {G2.14b} Users can delete NA values:
  expect_no_error(
    ww_unsystematic_rmpd(missing_df, y, x, na_action = na.omit)
  )

  #' @srrstats {G2.14b} Users can delete NA values:
  expect_no_error(
    ww_unsystematic_rmpd_vec(missing_df$x, missing_df$y, na_action = na.omit)
  )

  #' @srrstats {G2.14b} Users can delete NA values:
  expect_no_error(
    ww_unsystematic_rmpd_vec(missing_df$y, missing_df$x, na_action = na.omit)
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8a} Zero-length data:
  expect_snapshot(
    ww_unsystematic_rmpd_vec(numeric(), numeric()),
    error = TRUE
  )

  empty_df <- tibble::tibble(x = numeric(), y = numeric())
  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8a} Zero-length data:
  expect_snapshot(
    ww_unsystematic_rmpd(empty_df, x, y),
    error = TRUE
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8a} Zero-length data:
  expect_snapshot(
    ww_unsystematic_rmpd(empty_df, y, x),
    error = TRUE
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8c} All-NA:
  expect_snapshot(
    ww_unsystematic_rmpd_vec(rep(NA_real_, 4), 4:1),
    error = TRUE
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8c} All-NA:
  expect_snapshot(
    ww_unsystematic_rmpd_vec(1:4, rep(NA_real_, 4)),
    error = TRUE
  )

  all_na <- tibble::tibble(x = rep(NA_real_, 4), y = 1:4)
  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8c} All-NA:
  expect_snapshot(
    ww_unsystematic_rmpd(all_na, x, y),
    error = TRUE
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8c} All-NA:
  expect_snapshot(
    ww_unsystematic_rmpd(all_na, y, x),
    error = TRUE
  )

  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8c} All-identical:
  expect_snapshot(
    ww_unsystematic_rmpd_vec(1:4, 1:4)
  )

  all_identical <- tibble::tibble(x = 1:4, y = 1:4)
  #' @srrstats {G5.8} Edge condition tests
  #' @srrstats {G5.8c} All-identical:
  expect_snapshot(
    ww_unsystematic_rmpd(all_identical, x, y)
  )

})

test_that("other generic srr standards", {
  skip_if_not_installed("withr")
  x <- c(6, 8, 9, 10, 11, 14)
  y <- c(2, 3, 5, 5, 6, 8)
  df <- tibble::tibble(x = x, y = y)
  noised_x <- x + rnorm(x, .Machine$double.eps, .Machine$double.eps)
  noised_df <- tibble::tibble(x = noised_x, y = y)

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9a} Trivial noise doesn't change results:
  expect_equal(
    ww_unsystematic_rmpd(noised_df, x, y),
    ww_unsystematic_rmpd(df, x, y)
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9a} Trivial noise doesn't change results:
  expect_equal(
    ww_unsystematic_rmpd(noised_df, y, x),
    ww_unsystematic_rmpd(df, y, x)
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9a} Trivial noise doesn't change results:
  expect_equal(
    ww_unsystematic_rmpd_vec(noised_x, y),
    ww_unsystematic_rmpd_vec(x, y)
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9a} Trivial noise doesn't change results:
  expect_equal(
    ww_unsystematic_rmpd_vec(y, noised_x),
    ww_unsystematic_rmpd_vec(y, x)
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9b} Different seeds are equivalent:
  expect_equal(
    withr::with_seed(
      123,
      ww_unsystematic_rmpd(df, x, y)
    ),
    withr::with_seed(
      1107,
      ww_unsystematic_rmpd(df, x, y)
    )
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9b} Different seeds are equivalent:
  expect_equal(
    withr::with_seed(
      123,
      ww_unsystematic_rmpd(df, y, x)
    ),
    withr::with_seed(
      1107,
      ww_unsystematic_rmpd(df, y, x)
    )
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9b} Different seeds are equivalent:
  expect_equal(
    withr::with_seed(
      123,
      ww_unsystematic_rmpd_vec(x, y)
    ),
    withr::with_seed(
      1107,
      ww_unsystematic_rmpd_vec(x, y)
    )
  )

  #' @srrstats {G3.0} Testing with appropriate tolerances.
  #' @srrstats {G5.9} Noise susceptibility tests
  #' @srrstats {G5.9b} Different seeds are equivalent:
  expect_equal(
    withr::with_seed(
      123,
      ww_unsystematic_rmpd_vec(y, x)
    ),
    withr::with_seed(
      1107,
      ww_unsystematic_rmpd_vec(y, x)
    )
  )

})