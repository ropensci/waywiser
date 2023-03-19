#' Willmott's d and related values
#'
#' These functions calculate Willmott's d value, a proposed replacement for R2
#' which better differentiates between types and magnitudes of possible
#' covariations. Additional functions calculate systematic and unsystematic
#' components of MSE and RMSE; the sum of the systematic and unsystematic
#' components of MSE equal total MSE (though the same is not true for RMSE).
#'
#' Values of d and d1 range from 0 to 1, with 1 indicating perfect agreement.
#' Values of
#' dr range from -1 to 1, with 1 similarly indicating perfect agreement. Values
#' of RMSE are in the same units as `truth` and `estimate`, while values of MSE
#' are in squared units. `truth` and `estimate` must be the same length. This
#' function is not explicitly spatial and as such can be applied to data with
#' any number of dimensions and any coordinate reference system.
#'
#' @inheritParams yardstick::rmse
#' @inheritParams ww_area_of_applicability
#'
#' @return
#' A tibble with columns .metric, .estimator, and .estimate and 1 row of values.
#' For grouped data frames, the number of rows returned will be the same as the number of groups.
#' For `_vec()` functions, a single value (or NA).
#'
#' @family agreement metrics
#' @family yardstick metrics
#'
#' @examples
#' x <- c(6, 8, 9, 10, 11, 14)
#' y <- c(2, 3, 5, 5, 6, 8)
#'
#' ww_willmott_d_vec(x, y)
#' ww_willmott_d1_vec(x, y)
#' ww_willmott_dr_vec(x, y)
#' ww_systematic_mse_vec(x, y)
#' ww_unsystematic_mse_vec(x, y)
#' ww_systematic_rmse_vec(x, y)
#' ww_unsystematic_rmse_vec(x, y)
#'
#' example_df <- data.frame(x = x, y = y)
#' ww_willmott_d(example_df, x, y)
#' ww_willmott_d1(example_df, x, y)
#' ww_willmott_dr(example_df, x, y)
#' ww_systematic_mse(example_df, x, y)
#' ww_unsystematic_mse(example_df, x, y)
#' ww_systematic_rmse(example_df, x, y)
#' ww_unsystematic_rmse(example_df, x, y)
#'
#' @references
#' Willmott, C. J. 1981. "On the Validation of Models". Physical Geography 2(2),
#' pp 184-194, doi: 10.1080/02723646.1981.10642213.
#'
#' Willmott, C. J. 1982. "Some Comments on the Evaluation of Model Performance".
#' Bulletin of the American Meteorological Society 63(11), pp 1309-1313,
#' doi: 10.1175/1520-0477(1982)063<1309:SCOTEO>2.0.CO;2.
#'
#' Willmott C. J., Ackleson S. G., Davis R. E., Feddema J. J., Klink K. M.,
#' Legates D. R., O’Donnell J., Rowe C. M. 1985. "Statistics for the
#' evaluation of model performance." Journal of Geophysical Research
#' 90(C5): 8995–9005, doi: 10.1029/jc090ic05p08995
#'
#' Willmott, C. J., Robeson, S. M., and Matsuura, K. "A refined index of model
#' performance". International Journal of Climatology 32, pp 2088-2094, doi:
#' 10.1002/joc.2419.
#'
#' @export
ww_willmott_d <- function(data, ...) {
  UseMethod("ww_willmott_d")
}

ww_willmott_d <- new_numeric_metric(ww_willmott_d, direction = "maximize")

#' @rdname ww_willmott_d
#' @export
ww_willmott_d.data.frame <- function(data,
                                     truth,
                                     estimate,
                                     na_rm = TRUE,
                                     ...) {
  yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    na_rm = na_rm,
    name = "willmott_d",
    ...
  )
}

#' @rdname ww_willmott_d
#' @export
ww_willmott_d_vec <- function(truth,
                              estimate,
                              na_rm = TRUE,
                              ...) {
  ww_willmott_d_impl <- function(truth, estimate, ...) {
    numerator <- calc_ssd(truth, estimate)
    denominator <- sum(
      (abs(truth - mean(truth)) + abs(estimate - mean(truth)))^2
    )
    1 - (numerator / denominator)
  }

  yardstick_vec(
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    impl = ww_willmott_d_impl,
    ...
  )
}

#' @rdname ww_willmott_d
#' @export
ww_willmott_d1 <- function(data, ...) {
  UseMethod("ww_willmott_d1")
}

ww_willmott_d1 <- new_numeric_metric(ww_willmott_d1, direction = "maximize")

#' @rdname ww_willmott_d
#' @export
ww_willmott_d1.data.frame <- function(data,
                                      truth,
                                      estimate,
                                      na_rm = TRUE,
                                      ...) {
  yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    na_rm = na_rm,
    name = "willmott_d1",
    ...
  )
}

#' @rdname ww_willmott_d
#' @export
ww_willmott_d1_vec <- function(truth,
                               estimate,
                               na_rm = TRUE,
                               ...) {
  ww_willmott_d1_impl <- function(truth, estimate, ...) {
    numerator <- sum(abs(truth - estimate))
    denominator <- sum(
      (abs(truth - mean(truth)) + abs(estimate - mean(truth)))
    )
    1 - (numerator / denominator)
  }

  yardstick_vec(
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    impl = ww_willmott_d1_impl,
    ...
  )
}

#' @rdname ww_willmott_d
#' @export
ww_willmott_dr <- function(data, ...) {
  UseMethod("ww_willmott_dr")
}

ww_willmott_dr <- new_numeric_metric(ww_willmott_dr, direction = "maximize")

#' @rdname ww_willmott_d
#' @export
ww_willmott_dr.data.frame <- function(data,
                                      truth,
                                      estimate,
                                      na_rm = TRUE,
                                      ...) {
  yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    na_rm = na_rm,
    name = "willmott_dr",
    ...
  )
}

#' @rdname ww_willmott_d
#' @export
ww_willmott_dr_vec <- function(truth,
                               estimate,
                               na_rm = TRUE,
                               ...) {
  ww_willmott_dr_impl <- function(truth, estimate, ...) {
    term_1 <- sum(abs(estimate - truth))
    term_2 <- sum(abs(truth - mean(truth))) * 2

    if (term_1 <= term_2) {
      1 - (term_1 / term_2)
    } else {
      (term_2 / term_1) - 1
    }
  }

  yardstick_vec(
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    impl = ww_willmott_dr_impl,
    ...
  )
}

#' @rdname ww_willmott_d
#' @export
ww_systematic_mse <- function(data, ...) {
  UseMethod("ww_systematic_mse")
}

ww_systematic_mse <- new_numeric_metric(ww_systematic_mse, direction = "minimize")

#' @rdname ww_willmott_d
#' @export
ww_systematic_mse.data.frame <- function(data,
                                         truth,
                                         estimate,
                                         na_rm = TRUE,
                                         ...) {
  yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    na_rm = na_rm,
    name = "systematic_mse",
    ...
  )
}

#' @rdname ww_willmott_d
#' @export
ww_systematic_mse_vec <- function(truth,
                                  estimate,
                                  na_rm = TRUE,
                                  ...) {
  yardstick_vec(
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    impl = ww_systematic_mse_impl,
    ...
  )
}

ww_systematic_mse_impl <- function(truth, estimate, ...) {
  dt <- data.frame(truth = truth, estimate = estimate)
  preds <- predict(stats::lm(truth ~ estimate, dt), dt)

  mean((preds - truth)^2)
}

#' @rdname ww_willmott_d
#' @export
ww_unsystematic_mse <- function(data, ...) {
  UseMethod("ww_unsystematic_mse")
}

ww_unsystematic_mse <- new_numeric_metric(ww_unsystematic_mse, direction = "minimize")

#' @rdname ww_willmott_d
#' @export
ww_unsystematic_mse.data.frame <- function(data,
                                           truth,
                                           estimate,
                                           na_rm = TRUE,
                                           ...) {
  yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    na_rm = na_rm,
    name = "unsystematic_mse",
    ...
  )
}

#' @rdname ww_willmott_d
#' @export
ww_unsystematic_mse_vec <- function(truth,
                                    estimate,
                                    na_rm = TRUE,
                                    ...) {
  yardstick_vec(
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    impl = ww_unsystematic_mse_impl,
    ...
  )
}

ww_unsystematic_mse_impl <- function(truth, estimate, ...) {
  dt <- data.frame(truth = truth, estimate = estimate)
  preds <- predict(stats::lm(truth ~ estimate, dt), dt)

  mean((estimate - preds)^2)
}

#' @rdname ww_willmott_d
#' @export
ww_systematic_rmse <- function(data, ...) {
  UseMethod("ww_systematic_rmse")
}

ww_systematic_rmse <- new_numeric_metric(ww_systematic_rmse, direction = "minimize")

#' @rdname ww_willmott_d
#' @export
ww_systematic_rmse.data.frame <- function(data,
                                          truth,
                                          estimate,
                                          na_rm = TRUE,
                                          ...) {
  yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    na_rm = na_rm,
    name = "systematic_rmse",
    ...
  )
}

#' @rdname ww_willmott_d
#' @export
ww_systematic_rmse_vec <- function(truth,
                                   estimate,
                                   na_rm = TRUE,
                                   ...) {
  ww_systematic_rmse_impl <- function(truth, estimate, ...) {
    sqrt(ww_systematic_mse_impl(truth, estimate, ...))
  }

  yardstick_vec(
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    impl = ww_systematic_rmse_impl,
    ...
  )
}

#' @rdname ww_willmott_d
#' @export
ww_unsystematic_rmse <- function(data, ...) {
  UseMethod("ww_unsystematic_rmse")
}

ww_unsystematic_rmse <- new_numeric_metric(ww_unsystematic_rmse, direction = "minimize")

#' @rdname ww_willmott_d
#' @export
ww_unsystematic_rmse.data.frame <- function(data,
                                            truth,
                                            estimate,
                                            na_rm = TRUE,
                                            ...) {
  yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    na_rm = na_rm,
    name = "unsystematic_rmse",
    ...
  )
}

#' @rdname ww_willmott_d
#' @export
ww_unsystematic_rmse_vec <- function(truth,
                                     estimate,
                                     na_rm = TRUE,
                                     ...) {
  ww_unsystematic_rmse_impl <- function(truth, estimate, ...) {
    sqrt(ww_unsystematic_mse_impl(truth, estimate, ...))
  }

  yardstick_vec(
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    impl = ww_unsystematic_rmse_impl,
    ...
  )
}
