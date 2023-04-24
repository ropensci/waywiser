test_that("srr: {{{name}}} errors if truth and estimate are different lengths", {
  # Note that this test isn't applicable to data-frame input, which enforces
  # constant column lengths
  expect_snapshot(
    {{{name}}}_vec(1:5, 1:4),
    error = TRUE
  )
  expect_snapshot(
    {{{name}}}_vec(1:4, 1:5),
    error = TRUE
  )
})

test_that("srr: {{{name}}} errors if truth and estimate aren't numeric", {
  char_df <- tibble::tibble(x = 1:5, y = letters[1:5])
  expect_snapshot(
    {{{name}}}(char_df, x, y),
    error = TRUE
  )

  expect_snapshot(
    {{{name}}}(char_df, y, x),
    error = TRUE
  )

  expect_snapshot(
    {{{name}}}_vec(as.character(1:5), 1:4),
    error = TRUE
  )

  expect_snapshot(
    {{{name}}}_vec(1:5, as.character(1:4)),
    error = TRUE
  )
})

test_that("srr: {{{name}}} errors if truth and estimate are list columns", {
  list_df <- tibble::tibble(x = 1:5, y = lapply(1:5, function(x) x))
  expect_snapshot(
    {{{name}}}(list_df, x, y),
    error = TRUE
  )

  expect_snapshot(
    {{{name}}}(list_df, y, x),
    error = TRUE
  )
})

test_that("srr: {{{name}}} removes NaN and NA when na_rm = TRUE", {

  missing_df <- tibble::tibble(x = c(NaN, 2:5), y = c(1:4, NA))

  expect_snapshot(
    round({{{name}}}(missing_df, x, y)$.estimate, 15),
  )

  expect_snapshot(
    round({{{name}}}(missing_df, y, x)$.estimate, 15),
  )

  expect_snapshot(
    round({{{name}}}_vec(missing_df$y, missing_df$x), 15),
  )

  expect_snapshot(
    round({{{name}}}_vec(missing_df$x, missing_df$y), 15),
  )
})

test_that("srr: {{{name}}} returns NA when na_rm = FALSE and NA is present", {

  missing_df <- tibble::tibble(x = c(NaN, 2:5), y = c(1:4, NA))

  expect_identical(
    {{{name}}}(missing_df, y, x, na_rm = FALSE)$.estimate,
    NA_real_
  )

  expect_identical(
    {{{name}}}(missing_df, x, y, na_rm = FALSE)$.estimate,
    NA_real_
  )

  expect_identical(
    {{{name}}}_vec(missing_df$y, missing_df$x, na_rm = FALSE),
    NA_real_
  )

  expect_identical(
    {{{name}}}_vec(missing_df$x, missing_df$y, na_rm = FALSE),
    NA_real_
  )

})

test_that("srr: {{{name}}} errors on zero-length data", {

  expect_snapshot(
    {{{name}}}_vec(numeric(), numeric()),
    error = TRUE
  )

  empty_df <- tibble::tibble(x = numeric(), y = numeric())
  expect_snapshot(
    {{{name}}}(empty_df, x, y),
    error = TRUE
  )

  expect_snapshot(
    {{{name}}}(empty_df, y, x),
    error = TRUE
  )
})

test_that("srr: {{{name}}} errors on all-NA data", {

  expect_snapshot(
    {{{name}}}_vec(rep(NA_real_, 4), 4:1),
    error = TRUE
  )

  expect_snapshot(
    {{{name}}}_vec(1:4, rep(NA_real_, 4)),
    error = TRUE
  )

  all_na <- tibble::tibble(x = rep(NA_real_, 4), y = 1:4)
  expect_snapshot(
    {{{name}}}(all_na, x, y),
    error = TRUE
  )

  expect_snapshot(
    {{{name}}}(all_na, y, x),
    error = TRUE
  )

  expect_snapshot(
    {{{name}}}_vec(1:4, 1:4)
  )

})


test_that("srr: {{{name}}} works with all identical data", {

  all_identical <- tibble::tibble(x = 1:4, y = 1:4)
  expect_snapshot(
    {{{name}}}(all_identical, x, y)
  )

  expect_snapshot(
    {{{name}}}_vec(1:4, 1:4)
  )

  all_identical <- tibble::tibble(x = 1:4, y = 1:4)
  expect_snapshot(
    {{{name}}}(all_identical, x, y)
  )

})

test_that("srr: {{{name}}} results don't change with trivial noise", {
  skip_if_not_installed("withr")
  x <- c(6, 8, 9, 10, 11, 14)
  y <- c(2, 3, 5, 5, 6, 8)
  df <- tibble::tibble(x = x, y = y)
  noised_x <- x + rnorm(x, .Machine$double.eps, .Machine$double.eps)
  noised_df <- tibble::tibble(x = noised_x, y = y)

  expect_equal(
    {{{name}}}(noised_df, x, y),
    {{{name}}}(df, x, y)
  )

  expect_equal(
    {{{name}}}(noised_df, y, x),
    {{{name}}}(df, y, x)
  )

  expect_equal(
    {{{name}}}_vec(noised_x, y),
    {{{name}}}_vec(x, y)
  )

  expect_equal(
    {{{name}}}_vec(y, noised_x),
    {{{name}}}_vec(y, x)
  )

})

test_that("srr: {{{name}}} results don't change with different seeds", {
  skip_if_not_installed("withr")
  x <- c(6, 8, 9, 10, 11, 14)
  y <- c(2, 3, 5, 5, 6, 8)
  df <- tibble::tibble(x = x, y = y)

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
