#' Willmott's d and related values
#'
#' These functions calculate Willmott's d value, a proposed replacement for R2
#' which better differentiates between types and magnitudes of possible
#' covariations. Additional functions calculate systematic and unsystematic
#' components of MSE and RMSE; the sum of the systematic and unsystematic
#' components of MSE equal total MSE (though the same is not true for RMSE).
#'
#' Values of d range from 0 to 1, with 1 indicating perfect agreement. Values of
#' RMSE are in the same units as `truth` and `estimate`, while values of MSE are
#' squared.
#'
#' @inheritParams yardstick::rmse
#'
#' @return
#' A tibble with columns .metric, .estimator, and .estimate and `nrow(data)` rows of values.
#' For grouped data frames, the number of rows returned will be the same as the number of groups.
#' For `_vec()` functions, a single value (or NA).
#'
#' @family agreement metrics
#'
#' @examples
#' x <- c(6, 8, 9, 10, 11, 14)
#' y <- c(2, 3, 5, 5, 6, 8)
#'
#' ww_willmott_d_vec(x, y)
#' ww_systematic_mse_vec(x, y)
#' ww_unsystematic_mse_vec(x, y)
#'
#' @references
#' Willmott, C. J. 1981. "On the Validation of Models". Physical Geography 2(2),
#' pp 184-194, doi: 10.1080/02723646.1981.10642213.
#'
#' Willmott, C. J. 1982. "Some Comments on the Evaluation of Model Performance".
#' Bulletin of the American Meteorological Society 63(11), pp 1309-1313,
#' doi: 10.1175/1520-0477(1982)063<1309:SCOTEO>2.0.CO;2.
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
  metric_summarizer(
    metric_nm = "willmott_d",
    metric_fn = ww_willmott_d_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    na_rm = na_rm,
    metric_fn_options = list(...)
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

  metric_vec_template(
    metric_impl = ww_willmott_d_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
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
  metric_summarizer(
    metric_nm = "systematic_mse",
    metric_fn = ww_systematic_mse_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    na_rm = na_rm,
    metric_fn_options = list(...)
  )
}

#' @rdname ww_willmott_d
#' @export
ww_systematic_mse_vec <- function(truth,
                                  estimate,
                                  na_rm = TRUE,
                                  ...) {

  ww_systematic_mse_impl <- function(truth, estimate, ...) {
    dt <- data.frame(truth = truth, estimate = estimate)
    preds <- predict(lm(truth ~ estimate, dt), dt)

    mean((preds - truth)^2)
  }

  metric_vec_template(
    metric_impl = ww_systematic_mse_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
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
  metric_summarizer(
    metric_nm = "unsystematic_mse",
    metric_fn = ww_unsystematic_mse_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    na_rm = na_rm,
    metric_fn_options = list(...)
  )
}

#' @rdname ww_willmott_d
#' @export
ww_unsystematic_mse_vec <- function(truth,
                                    estimate,
                                    na_rm = TRUE,
                                    ...) {

  ww_unsystematic_mse_impl <- function(truth, estimate, ...) {
    dt <- data.frame(truth = truth, estimate = estimate)
    preds <- predict(lm(truth ~ estimate, dt), dt)

    mean((estimate - preds)^2)
  }

  metric_vec_template(
    metric_impl = ww_unsystematic_mse_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
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
  metric_summarizer(
    metric_nm = "systematic_rmse",
    metric_fn = ww_systematic_rmse_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    na_rm = na_rm,
    metric_fn_options = list(...)
  )
}

#' @rdname ww_willmott_d
#' @export
ww_systematic_rmse_vec <- function(truth,
                                  estimate,
                                  na_rm = TRUE,
                                  ...) {

  ww_systematic_rmse_impl <- function(truth, estimate, ...) {
    dt <- data.frame(truth = truth, estimate = estimate)
    preds <- predict(lm(truth ~ estimate, dt), dt)

    sqrt(mean((preds - truth)^2))
  }

  metric_vec_template(
    metric_impl = ww_systematic_rmse_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
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
  metric_summarizer(
    metric_nm = "unsystematic_rmse",
    metric_fn = ww_unsystematic_rmse_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    na_rm = na_rm,
    metric_fn_options = list(...)
  )
}

#' @rdname ww_willmott_d
#' @export
ww_unsystematic_rmse_vec <- function(truth,
                                    estimate,
                                    na_rm = TRUE,
                                    ...) {

  ww_unsystematic_rmse_impl <- function(truth, estimate, ...) {
    dt <- data.frame(truth = truth, estimate = estimate)
    preds <- predict(lm(truth ~ estimate, dt), dt)

    sqrt(mean((estimate - preds)^2))
  }

  metric_vec_template(
    metric_impl = ww_unsystematic_rmse_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

