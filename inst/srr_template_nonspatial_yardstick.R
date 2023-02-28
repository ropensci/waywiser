test_that("srr: expected failures for {{{name}}}", {
  # Note that this test isn't applicable to data-frame input, which enforces
  # constant column lengths
  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G2.0} Truth and estimate are equal in length:
  expect_snapshot(
    {{{name}}}_vec(1:5, 1:4),
    error = TRUE
  )

  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G2.0} Truth and estimate are equal in length:
  expect_snapshot(
    {{{name}}}_vec(1:4, 1:5),
    error = TRUE
  )

  char_df <- tibble::tibble(x = 1:5, y = letters[1:5])
  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.1} Truth and estimate are numeric:
  expect_snapshot(
    {{{name}}}(char_df, x, y),
    error = TRUE
  )

  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.1} Truth and estimate are numeric:
  expect_snapshot(
    {{{name}}}(char_df, y, x),
    error = TRUE
  )

  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.1} Truth and estimate are numeric:
  expect_snapshot(
    {{{name}}}_vec(as.character(1:5), 1:4),
    error = TRUE
  )

  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.1} Truth and estimate are numeric:
  expect_snapshot(
    {{{name}}}_vec(1:5, as.character(1:4)),
    error = TRUE
  )

  list_df <- tibble::tibble(x = 1:5, y = lapply(1:5, function(x) x))
  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.12} List column inputs fail:
  expect_snapshot(
    {{{name}}}(list_df, x, y),
    error = TRUE
  )

  #' @srrstats {G5.2} Testing errors
  #' @srrstats {G5.2b} Testing errors
  #' @srrstats {G5.8b} Data of unsupported types
  #' @srrstats {G2.12} List column inputs fail:
  expect_snapshot(
    {{{name}}}(list_df, y, x),
    error = TRUE
  )

  #' @srrstats {G2.13} Missing data is properly handled
  #' @srrstats {G2.15} Missingness is checked
  #' @srrstats {G2.14} Users can specify behavior with NA results
  #' @srrstats {G2.16} NaN is properly handled
  missing_df <- tibble::tibble(x = c(NaN, 2:5), y = c(1:4, NA))
  #' Users can remove NA:
  expect_snapshot(
    round({{{name}}}(missing_df, x, y)$.estimate, 15),
  )

  #' Users can remove NA:
  expect_snapshot(
    round({{{name}}}(missing_df, y, x)$.estimate, 15),
  )

  #' Users can remove NA:
  expect_snapshot(
    round({{{name}}}_vec(missing_df$y, missing_df$x), 15),
  )

  #' Users can remove NA:
  expect_snapshot(
    round({{{name}}}_vec(missing_df$x, missing_df$y), 15),
  )

  #' @srrstats {G2.14b} Users can ignore NA:
  expect_identical(
    {{{name}}}(missing_df, y, x, na_rm = FALSE)$.estimate,
    NA_real_
  )

  #' @srrstats {G2.14b} Users can ignore NA:
  expect_identical(
    {{{name}}}(missing_df, x, y, na_rm = FALSE)$.estimate,
    NA_real_
  )

  #' @srrstats {G2.14b} Users can ignore NA:
  expect_identical(
    {{{name}}}_vec(missing_df$y, missing_df$x, na_rm = FALSE),
    NA_real_
  )

  #' @srrstats {G2.14b} Users can ignore NA:
  expect_identical(
    {{{name}}}_vec(missing_df$x, missing_df$y, na_rm = FALSE),
    NA_real_
  )

  # Edge condition tests: Zero-length data:
  expect_snapshot(
    {{{name}}}_vec(numeric(), numeric()),
    error = TRUE
  )

  empty_df <- tibble::tibble(x = numeric(), y = numeric())
  # Edge condition tests: Zero-length data:
  expect_snapshot(
    {{{name}}}(empty_df, x, y),
    error = TRUE
  )

  # Edge condition tests: Zero-length data:
  expect_snapshot(
    {{{name}}}(empty_df, y, x),
    error = TRUE
  )

  # Edge condition tests: All-NA:
  expect_snapshot(
    {{{name}}}_vec(rep(NA_real_, 4), 4:1),
    error = TRUE
  )

  # Edge condition tests: All-NA:
  expect_snapshot(
    {{{name}}}_vec(1:4, rep(NA_real_, 4)),
    error = TRUE
  )

  all_na <- tibble::tibble(x = rep(NA_real_, 4), y = 1:4)
  # Edge condition tests: All-NA:
  expect_snapshot(
    {{{name}}}(all_na, x, y),
    error = TRUE
  )

  # Edge condition tests: All-NA:
  expect_snapshot(
    {{{name}}}(all_na, y, x),
    error = TRUE
  )

  # Edge condition tests: All-identical:
  expect_snapshot(
    {{{name}}}_vec(1:4, 1:4)
  )

  all_identical <- tibble::tibble(x = 1:4, y = 1:4)
  # Edge condition tests: All-identical:
  expect_snapshot(
    {{{name}}}(all_identical, x, y)
  )

})

test_that("other generic srr standards", {
  skip_if_not_installed("withr")
  x <- c(6, 8, 9, 10, 11, 14)
  y <- c(2, 3, 5, 5, 6, 8)
  df <- tibble::tibble(x = x, y = y)
  noised_x <- x + rnorm(x, .Machine$double.eps, .Machine$double.eps)
  noised_df <- tibble::tibble(x = noised_x, y = y)

  # Noise susceptibility tests: Trivial noise doesn't change results:
  expect_equal(
    {{{name}}}(noised_df, x, y),
    {{{name}}}(df, x, y)
  )

  # Noise susceptibility tests: Trivial noise doesn't change results:
  expect_equal(
    {{{name}}}(noised_df, y, x),
    {{{name}}}(df, y, x)
  )

  # Noise susceptibility tests: Trivial noise doesn't change results:
  expect_equal(
    {{{name}}}_vec(noised_x, y),
    {{{name}}}_vec(x, y)
  )

  # Noise susceptibility tests: Trivial noise doesn't change results:
  expect_equal(
    {{{name}}}_vec(y, noised_x),
    {{{name}}}_vec(y, x)
  )

  # Noise susceptibility tests: Different seeds are equivalent:
  expect_equal(
    withr::with_seed(
      123,
      {{{name}}}(df, x, y)
    ),
    withr::with_seed(
      1107,
      {{{name}}}(df, x, y)
    )
  )

  # Noise susceptibility tests: Different seeds are equivalent:
  expect_equal(
    withr::with_seed(
      123,
      {{{name}}}(df, y, x)
    ),
    withr::with_seed(
      1107,
      {{{name}}}(df, y, x)
    )
  )

  # Noise susceptibility tests: Different seeds are equivalent:
  expect_equal(
    withr::with_seed(
      123,
      {{{name}}}_vec(x, y)
    ),
    withr::with_seed(
      1107,
      {{{name}}}_vec(x, y)
    )
  )

  # Noise susceptibility tests: Different seeds are equivalent:
  expect_equal(
    withr::with_seed(
      123,
      {{{name}}}_vec(y, x)
    ),
    withr::with_seed(
      1107,
      {{{name}}}_vec(y, x)
    )
  )

})
