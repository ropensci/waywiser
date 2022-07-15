#' Global Moran's I statistic
#'
#' Calculate the global Moran's I statistic for model residuals.
#' `ww_global_moran_i()` returns the statistic itself, while
#' `ww_global_moran_pvalue()` returns the associated p value.
#' `ww_global_moran()` returns both.
#'
#' @inheritParams yardstick::rmse
#' @inheritParams spdep::moran.test
#' @param wt A "listw" object, for instance as created with [ww_build_weights()].
#' @param randomization variance of I calculated under the assumption of randomisation, if FALSE normality
#' @param ... Additional arguments passed to [spdep::moran.test()].
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
#' ww_global_moran_i(guerry_modeled, crime_pers, predictions)
#' ww_global_moran(guerry_modeled, crime_pers, predictions)
#'
#' @rdname global_moran_i
#' @export
ww_global_moran_i <- function(data, ...) {
  UseMethod("ww_global_moran_i")
}

ww_global_moran_i <- new_numeric_metric(ww_global_moran_i, direction = "zero")

#' @export
ww_global_moran_i.data.frame <- function(data,
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
    metric_nm = "global_moran_i",
    metric_fn = ww_global_moran_i_vec,
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

#' @rdname global_moran_i
#' @export
ww_global_moran_i_vec <- function(truth,
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

  ww_global_moran_i_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    spdep::moran.test(
      x = resid,
      listw = wt,
      alternative = alternative,
      randomisation = randomization,
      ...
    )$estimate[[1]]

  }

  metric_vec_template(
    metric_impl = ww_global_moran_i_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

#' @rdname global_moran_i
#' @export
ww_global_moran_pvalue <- function(data, ...) {
  UseMethod("ww_global_moran_pvalue")
}

ww_global_moran_pvalue <- new_numeric_metric(ww_global_moran_pvalue, "minimize")

#' @export
ww_global_moran_pvalue.data.frame <- function(data,
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
    metric_nm = "global_moran_pvalue",
    metric_fn = ww_global_moran_pvalue_vec,
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

#' @rdname global_moran_i
#' @export
ww_global_moran_pvalue_vec <- function(truth,
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

  ww_global_moran_pvalue_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    spdep::moran.test(
      x = resid,
      listw = wt,
      alternative = alternative,
      randomisation = randomization,
      ...
    )$p.value
  }

  metric_vec_template(
    metric_impl = ww_global_moran_pvalue_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

#' @rdname global_moran_i
#' @export
ww_global_moran <- function(data,
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

  metrics <- metric_set(ww_global_moran_i, ww_global_moran_pvalue)
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
