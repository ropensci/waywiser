#' Local Moran's I statistic
#'
#' Calculate the local Moran's I statistic for model residuals.
#' `ww_local_moran_i()` returns the statistic itself, while
#' `ww_local_moran_pvalue()` returns the associated p value.
#' `ww_local_moran()` returns both.
#'
#' @inheritParams yardstick::rmse
#' @inheritParams spdep::localmoran
#' @param wt A "listw" object, for instance as created with [ww_build_weights()].
#' @param ... Additional arguments passed to [spdep::localmoran()].
#'
#' @return
#' A tibble with columns .metric, .estimator, and .estimate and `nrow(data)` rows of values.
#' For grouped data frames, the number of rows returned will be the same as the number of groups.
#' For `_vec()` functions, a numeric vector of `length(truth)` (or NA).
#'
#' @examplesIf rlang::is_installed("sfdep")
#' data(guerry, package = "sfdep")
#'
#' guerry_modeled <- guerry
#' guerry_lm <- lm(crime_pers ~ literacy, guerry_modeled)
#' guerry_modeled$predictions <- predict(guerry_lm, guerry_modeled)
#'
#' ww_local_moran_i(guerry_modeled, crime_pers, predictions)
#' ww_local_moran(guerry_modeled, crime_pers, predictions)
#'
#' @rdname local_moran_i
#' @export
ww_local_moran_i <- function(data, ...) {
  UseMethod("ww_local_moran_i")
}

ww_local_moran_i <- new_numeric_metric(ww_local_moran_i, direction = "zero")

#' @export
ww_local_moran_i.data.frame <- function(data,
                                        truth,
                                        estimate,
                                        wt = NULL,
                                        alternative = "two.sided",
                                        na_rm = TRUE,
                                        ...) {

  if (is.null(wt)) {
    wt <- ww_build_weights(data)
  }
  if (rlang::is_function(wt)) {
    wt <- do.call(wt, list(data))
  }

  metric_summarizer(
    metric_nm = "local_moran_i",
    metric_fn = ww_local_moran_i_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    na_rm = na_rm,
    metric_fn_options = list(
      wt = wt,
      alternative = alternative,
      ...
    )
  )
}

#' @rdname local_moran_i
#' @export
ww_local_moran_i_vec <- function(truth,
                                 estimate,
                                 wt = NULL,
                                 alternative = "two.sided",
                                 na_rm = TRUE,
                                 ...) {

  if (!inherits(wt, "listw")) {
    rlang::abort(
      "`wt` must be a 'listw' object",
      "i" = "You can create 'listw' objects using `build_weights()`"
    )
  }

  ww_local_moran_i_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    spdep::localmoran(
      x = resid,
      listw = wt,
      alternative = alternative,
      ...
    )[, 1]

  }

  metric_vec_template(
    metric_impl = ww_local_moran_i_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

#' @rdname local_moran_i
#' @export
ww_local_moran_pvalue <- function(data, ...) {
  UseMethod("ww_local_moran_pvalue")
}

ww_local_moran_pvalue <- new_numeric_metric(ww_local_moran_pvalue, "minimize")

#' @export
ww_local_moran_pvalue.data.frame <- function(data,
                                             truth,
                                             estimate,
                                             wt = NULL,
                                             alternative = "two.sided",
                                             na_rm = TRUE,
                                             ...) {

  if (is.null(wt)) {
    wt <- ww_build_weights(data)
  }
  if (rlang::is_function(wt)) {
    wt <- do.call(wt, list(data))
  }

  metric_summarizer(
    metric_nm = "local_moran_pvalue",
    metric_fn = ww_local_moran_pvalue_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    na_rm = na_rm,
    metric_fn_options = list(
      wt = wt,
      alternative = alternative,
      ...
    )
  )
}

#' @rdname local_moran_i
#' @export
ww_local_moran_pvalue_vec <- function(truth,
                                      estimate,
                                      wt = NULL,
                                      alternative = "two.sided",
                                      na_rm = TRUE,
                                      ...) {

  if (!inherits(wt, "listw")) {
    rlang::abort(
      "`wt` must be a 'listw' object",
      "i" = "You can create 'listw' objects using `build_weights()`"
    )
  }

  ww_local_moran_pvalue_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    spdep::localmoran(
      x = resid,
      listw = wt,
      alternative = alternative,
      ...
    )[, 5]

  }

  metric_vec_template(
    metric_impl = ww_local_moran_pvalue_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

#' @rdname local_moran_i
#' @export
ww_local_moran <- function(data,
                           truth,
                           estimate,
                           wt = NULL,
                           alternative = "two.sided",
                           na_rm = TRUE,
                           ...) {

  if (is.null(wt)) {
    wt <- ww_build_weights(data)
  }
  if (rlang::is_function(wt)) {
    wt <- do.call(wt, list(data))
  }

  metrics <- metric_set(ww_local_moran_i, ww_local_moran_pvalue)
  metrics(
    data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    wt = wt,
    alternative = alternative,
    na_rm = na_rm,
    ...
  )
}
