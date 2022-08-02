#' Global Geary's C statistic
#'
#' Calculate the global Geary's C statistic for model residuals.
#' `ww_global_geary_c()` returns the statistic itself, while
#' `ww_global_geary_pvalue()` returns the associated p value.
#' `ww_global_geary()` returns both.
#'
#' @inheritParams yardstick::rmse
#' @inheritParams spdep::geary.test
#' @param wt A "listw" object, for instance as created with [ww_build_weights()].
#' @param randomization variance of I calculated under the assumption of randomisation, if FALSE normality
#' @param ... Additional arguments passed to [spdep::geary.test()].
#'
#' @return
#' A tibble with columns .metric, .estimator, and .estimate and `nrow(data)` rows of values.
#' For grouped data frames, the number of rows returned will be the same as the number of groups.
#' For `_vec()` functions, a single value (or NA).
#'
#' @examplesIf rlang::is_installed("sfdep")
#' data(guerry, package = "sfdep")
#'
#' guerry_modeled <- guerry
#' guerry_lm <- lm(crime_pers ~ literacy, guerry_modeled)
#' guerry_modeled$predictions <- predict(guerry_lm, guerry_modeled)
#'
#' \dontrun{
#' ww_global_geary(guerry_modeled, crime_pers, predictions)
#' }
#'
#' @rdname global_geary_c
#' @export
ww_global_geary_c <- function(data, ...) {
  UseMethod("ww_global_geary_c")
}

ww_global_geary_c <- new_numeric_metric(ww_global_geary_c, direction = "zero")

#' @export
ww_global_geary_c.data.frame <- function(data,
                                         truth,
                                         estimate,
                                         wt = NULL,
                                         alternative = "greater",
                                         randomization = TRUE,
                                         na_rm = TRUE,
                                         ...) {

  if (is.null(wt)) {
    wt <- ww_build_weights(data)
  }
  if (rlang::is_function(wt)) {
    wt <- do.call(wt, list(data))
  }

  metric_summarizer(
    metric_nm = "global_geary_c",
    metric_fn = ww_global_geary_c_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    na_rm = na_rm,
    metric_fn_options = list(
      wt = wt,
      alternative = "greater",
      randomization = TRUE,
      ...
    )
  )
}

#' @rdname global_geary_c
#' @export
ww_global_geary_c_vec <- function(truth,
                                  estimate,
                                  wt = NULL,
                                  alternative = "greater",
                                  randomization = TRUE,
                                  na_rm = TRUE,
                                  ...) {

  if (!inherits(wt, "listw")) {
    rlang::abort(
      "`wt` must be a 'listw' object",
      "i" = "You can create 'listw' objects using `build_weights()`"
    )
  }

  ww_global_geary_c_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    spdep::geary(
      x = resid,
      listw = wt,
      length(wt[["neighbours"]]),
      length(wt[["neighbours"]]) - 1,
      spdep::Szero(wt),
      ...
    )$C
  }

  metric_vec_template(
    metric_impl = ww_global_geary_c_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

#' @rdname global_geary_c
#' @export
ww_global_geary_pvalue <- function(data, ...) {
  UseMethod("ww_global_geary_pvalue")
}

ww_global_geary_pvalue <- new_numeric_metric(ww_global_geary_pvalue, "minimize")

#' @export
ww_global_geary_pvalue.data.frame <- function(data,
                                              truth,
                                              estimate,
                                              wt = NULL,
                                              alternative = "greater",
                                              randomization = TRUE,
                                              na_rm = TRUE,
                                              ...) {

  if (is.null(wt)) {
    wt <- ww_build_weights(data)
  }
  if (rlang::is_function(wt)) {
    wt <- do.call(wt, list(data))
  }

  metric_summarizer(
    metric_nm = "global_geary_pvalue",
    metric_fn = ww_global_geary_pvalue_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    na_rm = na_rm,
    metric_fn_options = list(
      wt = wt,
      alternative = alternative,
      randomization = randomization,
      ...
    )
  )
}

#' @rdname global_geary_c
#' @export
ww_global_geary_pvalue_vec <- function(truth,
                                       estimate,
                                       wt = NULL,
                                       alternative = "greater",
                                       randomization = TRUE,
                                       na_rm = TRUE,
                                       ...) {

  if (!inherits(wt, "listw")) {
    rlang::abort(
      "`wt` must be a 'listw' object",
      "i" = "You can create 'listw' objects using `build_weights()`"
    )
  }

  ww_global_geary_pvalue_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    spdep::geary.test(
      x = resid,
      listw = wt,
      alternative = alternative,
      randomisation = randomization,
      ...
    )$p.value
  }

  metric_vec_template(
    metric_impl = ww_global_geary_pvalue_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

#' @rdname global_geary_c
#' @export
ww_global_geary <- function(data,
                            truth,
                            estimate,
                            wt = NULL,
                            alternative = "greater",
                            randomization = TRUE,
                            na_rm = TRUE,
                            ...) {

  if (is.null(wt)) {
    wt <- ww_build_weights(data)
  }
  if (rlang::is_function(wt)) {
    wt <- do.call(wt, list(data))
  }

  metrics <- metric_set(ww_global_geary_c, ww_global_geary_pvalue)
  metrics(
    data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    wt = wt,
    alternative = alternative,
    randomization = randomization,
    na_rm = na_rm,
    ...
  )
}
