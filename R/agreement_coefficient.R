#' Agreement coefficients and related methods
#'
#' These functions calculate the agreement coefficient and mean product
#' difference (MPD), as well as their systematic and unsystematic components,
#' from Ji and Gallo (2006). Agreement coefficients provides a useful
#' measurement of agreement between two data sets which is bounded, symmetrical,
#' and can be decomposed into systematic and unsystematic components;
#' however, it assumes a linear relationship between the two data sets and
#' treats both "truth" and "estimate" as being of equal quality, and as such may
#' not be a useful metric in all scenarios.
#'
#' Agreement coefficient values range from 0 to 1, with 1 indicating perfect
#' agreement. `truth` and `estimate` must be the same length.
#'
#' @srrstats {G2.0a} Lengths documented above.
#'
#' @srrstats {G1.4} roxygen2 documentation
#' @srrstats {G2.1a} This function uses yardstick's type documentation.
#' @srrstats {G2.2} This function uses yardstick's parameter checking.
#' @srrstats {G2.7} This function relies on yardstick and dplyr and therefore only handles data.frame and vector input.
#' @srrstats {G2.8} This function relies on yardstick's type validation, which ensures proper conversion.
#' @srrstats {G2.10} Column extraction is properly handled within yardstick.
#' @srrstats {G2.14} This function relies on yardstick's NA handling
#' @srrstats {G2.14a} This function relies on yardstick's NA handling
#' @srrstats {G2.14b} This function relies on yardstick's NA handling
#' @srrstats {G2.14c} This function relies on yardstick's NA handling
#' @srrstats {G2.15} This function relies on yardstick's NA handling
#' @srrstats {G2.16} This function relies on yardstick's missingness handling.
#'
#' @inheritParams yardstick::rmse
#' @inheritParams ww_area_of_applicability
#'
#' @return
#' A tibble with columns .metric, .estimator, and .estimate and `nrow(data)` rows of values.
#' For grouped data frames, the number of rows returned will be the same as the number of groups.
#' For `_vec()` functions, a single value (or NA).
#'
#' @family agreement metrics
#' @family yardstick metrics
#'
#' @srrstats {G5.1} Testing data is below.
#' @srrstats {G5.4c} Testing data is from the original paper.
#' @examples
#' # Calculated values match Ji and Gallo 2006:
#' x <- c(6, 8, 9, 10, 11, 14)
#' y <- c(2, 3, 5, 5, 6, 8)
#'
#' ww_agreement_coefficient_vec(x, y)
#' ww_systematic_agreement_coefficient_vec(x, y)
#' ww_unsystematic_agreement_coefficient_vec(x, y)
#' ww_systematic_mpd_vec(x, y)
#' ww_unsystematic_mpd_vec(x, y)
#' ww_systematic_rmpd_vec(x, y)
#' ww_unsystematic_rmpd_vec(x, y)
#'
#' @srrstats {G1.0} Reference for these methods:
#' @references
#' Ji, L. and Gallo, K. 2006. "An Agreement Coefficient for Image Comparison."
#' Photogrammetric Engineering & Remote Sensing 72(7), pp 823–833,
#' doi: 10.14358/PERS.72.7.823.
#'
#' @export
ww_agreement_coefficient <- function(data, ...) {
  UseMethod("ww_agreement_coefficient")
}

ww_agreement_coefficient <- new_numeric_metric(ww_agreement_coefficient, direction = "maximize")

#' @rdname ww_agreement_coefficient
#' @export
ww_agreement_coefficient.data.frame <- function(data,
                                                truth,
                                                estimate,
                                                na_action = na.fail,
                                                ...) {
  yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    na_action = na_action,
    name = "agreement_coefficient",
    ...
  )
}

#' @rdname ww_agreement_coefficient
#' @export
ww_agreement_coefficient_vec <- function(truth,
                                         estimate,
                                         na_action = na.fail,
                                         ...) {

  ww_agreement_coefficient_impl <- function(truth, estimate, ...) {
    est_SSD <- calc_ssd(truth, estimate)
    est_SPOD <- calc_spod(truth, estimate)
    1 - est_SSD / est_SPOD
  }

  yardstick_vec(
    truth = truth,
    estimate = estimate,
    na_action = na_action,
    impl = ww_agreement_coefficient_impl,
    ...
  )
}

#' @rdname ww_agreement_coefficient
#' @export
ww_systematic_agreement_coefficient <- function(data, ...) {
  UseMethod("ww_systematic_agreement_coefficient")
}

ww_systematic_agreement_coefficient <- new_numeric_metric(ww_systematic_agreement_coefficient, direction = "maximize")

#' @rdname ww_agreement_coefficient
#' @export
ww_systematic_agreement_coefficient.data.frame <- function(data,
                                                           truth,
                                                           estimate,
                                                           na_action = na.fail,
                                                           ...) {
  yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    na_action = na_action,
    name = "systematic_agreement_coefficient",
    ...
  )
}

#' @rdname ww_agreement_coefficient
#' @export
ww_systematic_agreement_coefficient_vec <- function(truth,
                                                    estimate,
                                                    na_action = na.fail,
                                                    ...) {

  ww_systematic_agreement_coefficient_impl <- function(truth, estimate, ...) {
    1 - (calc_spds(truth, estimate) / calc_spod(truth, estimate))
  }

  yardstick_vec(
    truth = truth,
    estimate = estimate,
    na_action = na_action,
    impl = ww_systematic_agreement_coefficient_impl,
    ...
  )
}

#' @rdname ww_agreement_coefficient
#' @export
ww_unsystematic_agreement_coefficient <- function(data, ...) {
  UseMethod("ww_unsystematic_agreement_coefficient")
}

ww_unsystematic_agreement_coefficient <- new_numeric_metric(ww_unsystematic_agreement_coefficient, direction = "maximize")

#' @rdname ww_agreement_coefficient
#' @export
ww_unsystematic_agreement_coefficient.data.frame <- function(data,
                                                             truth,
                                                             estimate,
                                                             na_action = na.fail,
                                                             ...) {
  yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    na_action = na_action,
    name = "unsystematic_agreement_coefficient",
    ...
  )
}

#' @rdname ww_agreement_coefficient
#' @export
ww_unsystematic_agreement_coefficient_vec <- function(truth,
                                                      estimate,
                                                      na_action = na.fail,
                                                      ...) {

  ww_unsystematic_agreement_coefficient_impl <- function(truth, estimate, ...) {
    1 - (calc_spdu(truth, estimate) / calc_spod(truth, estimate))
  }

  yardstick_vec(
    truth = truth,
    estimate = estimate,
    na_action = na_action,
    impl = ww_unsystematic_agreement_coefficient_impl,
    ...
  )
}

#' @rdname ww_agreement_coefficient
#' @export
ww_unsystematic_mpd <- function(data, ...) {
  UseMethod("ww_unsystematic_mpd")
}

ww_unsystematic_mpd <- new_numeric_metric(ww_unsystematic_mpd, direction = "maximize")

#' @rdname ww_agreement_coefficient
#' @export
ww_unsystematic_mpd.data.frame <- function(data,
                                           truth,
                                           estimate,
                                           na_action = na.fail,
                                           ...) {
  yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    na_action = na_action,
    name = "unsystematic_mpd",
    ...
  )
}

#' @rdname ww_agreement_coefficient
#' @export
ww_unsystematic_mpd_vec <- function(truth,
                                    estimate,
                                    na_action = na.fail,
                                    ...) {
  yardstick_vec(
    truth = truth,
    estimate = estimate,
    na_action = na_action,
    impl = ww_unsystematic_mpd_impl,
    ...
  )
}

ww_unsystematic_mpd_impl <- function(truth, estimate, ...) {
  calc_spdu(truth, estimate) / length(truth)
}

#' @rdname ww_agreement_coefficient
#' @export
ww_systematic_mpd <- function(data, ...) {
  UseMethod("ww_systematic_mpd")
}

ww_systematic_mpd <- new_numeric_metric(ww_systematic_mpd, direction = "maximize")

#' @rdname ww_agreement_coefficient
#' @export
ww_systematic_mpd.data.frame <- function(data,
                                         truth,
                                         estimate,
                                         na_action = na.fail,
                                         ...) {
  yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    na_action = na_action,
    name = "systematic_mpd",
    ...
  )
}

#' @rdname ww_agreement_coefficient
#' @export
ww_systematic_mpd_vec <- function(truth,
                                  estimate,
                                  na_action = na.fail,
                                  ...) {
  yardstick_vec(
    truth = truth,
    estimate = estimate,
    na_action = na_action,
    impl = ww_systematic_mpd_impl,
    ...
  )
}

ww_systematic_mpd_impl <- function(truth, estimate, ...) {
  calc_spds(truth, estimate) / length(truth)
}

#' @rdname ww_agreement_coefficient
#' @export
ww_unsystematic_rmpd <- function(data, ...) {
  UseMethod("ww_unsystematic_rmpd")
}

ww_unsystematic_rmpd <- new_numeric_metric(ww_unsystematic_rmpd, direction = "maximize")

#' @rdname ww_agreement_coefficient
#' @export
ww_unsystematic_rmpd.data.frame <- function(data,
                                            truth,
                                            estimate,
                                            na_action = na.fail,
                                            ...) {
  yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    na_action = na_action,
    name = "unsystematic_rmpd",
    ...
  )
}

#' @rdname ww_agreement_coefficient
#' @export
ww_unsystematic_rmpd_vec <- function(truth,
                                     estimate,
                                     na_action = na.fail,
                                     ...) {

  ww_unsystematic_rmpd_impl <- function(truth, estimate, ...) {
    sqrt(ww_unsystematic_mpd_impl(truth, estimate, ...))
  }

  yardstick_vec(
    truth = truth,
    estimate = estimate,
    na_action = na_action,
    impl = ww_unsystematic_rmpd_impl,
    ...
  )
}

#' @rdname ww_agreement_coefficient
#' @export
ww_systematic_rmpd <- function(data, ...) {
  UseMethod("ww_systematic_rmpd")
}

ww_systematic_rmpd <- new_numeric_metric(ww_systematic_rmpd, direction = "maximize")

#' @rdname ww_agreement_coefficient
#' @export
ww_systematic_rmpd.data.frame <- function(data,
                                          truth,
                                          estimate,
                                          na_action = na.fail,
                                          ...) {
  yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    na_action = na_action,
    name = "systematic_rmpd",
    ...
  )
}

#' @rdname ww_agreement_coefficient
#' @export
ww_systematic_rmpd_vec <- function(truth,
                                   estimate,
                                   na_action = na.fail,
                                   ...) {

  ww_systematic_rmpd_impl <- function(truth, estimate, ...) {
    sqrt(ww_systematic_mpd_impl(truth, estimate, ...))
  }

  yardstick_vec(
    truth = truth,
    estimate = estimate,
    na_action = na_action,
    impl = ww_systematic_rmpd_impl,
    ...
  )
}

#' Return the sum of squared differences
#'
#' @inheritParams yardstick::rmse
#' @srrstats {G1.4a} Documented internal functions
#'
#' @noRd
calc_ssd <- function(truth, estimate) sum((truth - estimate)^2)

#' Return Sum of Potential Difference from Ji and Gallo (2006)
#'
#' @inheritParams yardstick::rmse
#' @srrstats {G1.4a} Documented internal functions
#'
#' @noRd
calc_spod <- function(truth, estimate) {
  mean_truth <- mean(truth)
  mean_estimate <- mean(estimate)

  sum(
    (abs(mean_truth - mean_estimate) + abs(truth - mean_truth)) *
      (abs(mean_truth - mean_estimate) + abs(estimate - mean_estimate))
  )
}

#' Return the coefficients of the GMFR line from Ji and Gallo (2006)
#'
#' @inheritParams yardstick::rmse
#' @srrstats {G1.4a} Documented internal functions
#'
#' @noRd
gmfr <- function(truth, estimate) {
  mean_truth <- mean(truth)
  mean_estimate <- mean(estimate)

  correlation_sign <- sign(stats::cor(truth, estimate))

  b <- sqrt(
    sum((truth - mean_truth)^2) /
      sum((estimate - mean_estimate)^2)
  )

  b <- abs(b) * correlation_sign

  a <- mean_truth - (b * mean_estimate)

  list(
    a = a,
    b = b
  )
}

#' Return the unsystematic sum product-difference from Ji and Gallo (2006)
#'
#' @inheritParams yardstick::rmse
#' @srrstats {G1.4a} Documented internal functions
#'
#' @noRd
calc_spdu <- function(truth, estimate) {
  mean_truth <- mean(truth)
  mean_estimate <- mean(estimate)

  gmfr_predict_truth <- gmfr(truth, estimate)
  gmfr_predict_estimate <- gmfr(estimate, truth)

  predicted_truth <- gmfr_predict_truth$a +
    (gmfr_predict_truth$b * estimate)
  predicted_estimate <- gmfr_predict_estimate$a +
    (gmfr_predict_estimate$b * truth)

  sum(
    abs(estimate - predicted_estimate) *
      abs(truth - predicted_truth)
  )
}

#' Return the systematic sum product-difference from Ji and Gallo (2006)
#'
#' @inheritParams yardstick::rmse
#' @srrstats {G1.4a} Documented internal functions
#'
#' @noRd
calc_spds <- function(truth, estimate) {
  est_spdu <- calc_spdu(truth, estimate)
  est_ssd <- calc_ssd(truth, estimate)
  est_ssd - est_spdu
}
