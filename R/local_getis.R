#' Local Getis-Ord G and G* statistic
#'
#' Calculate the local Getis-Ord G and G* statistic for model residuals.
#' `ww_local_getis_ord_g()` returns the statistic itself, while
#' `ww_local_getis_ord_pvalue()` returns the associated p value.
#' `ww_local_getis_ord()` returns both.
#'
#' @inheritParams yardstick::rmse
#' @inheritParams spdep::localG_perm
#' @param wt A "listw" object, for instance as created with [ww_build_weights()].
#' @param ... Arguments passed to [spdep::localG_perm()]
#' @inheritParams ww_build_weights
#' @param include_self Include each region itself in its own list of neighbors?
#' Only used when `wt` is `NULL`, and if `TRUE` means this function calculates
#' G* instead of G.
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
#' ww_local_getis_ord_g(guerry_modeled, crime_pers, predictions)
#' ww_local_getis_ord(guerry_modeled, crime_pers, predictions)
#' ww_local_getis_ord(guerry_modeled, crime_pers, predictions, include_self = TRUE)
#'
#' @rdname local_getis_ord_g
#' @export
ww_local_getis_ord_g <- function(data, ...) {
  UseMethod("ww_local_getis_ord_g")
}

ww_local_getis_ord_g <- new_numeric_metric(ww_local_getis_ord_g, direction = "zero")

#' @export
ww_local_getis_ord_g.data.frame <- function(data,
                                            truth,
                                            estimate,
                                            wt = NULL,
                                            alternative = "two.sided",
                                            nsim = 499,
                                            na_rm = TRUE,
                                            ...,
                                            include_self = FALSE) {

  if (is.null(wt)) {
    wt <- ww_build_weights(data, include_self = include_self)
  }
  if (rlang::is_function(wt)) {
    wt <- do.call(wt, list(data))
  }
  metric_nm <- "local_getis_ord_g"
  if (identical(attr(wt$neighbours, "self.included"), TRUE)) metric_nm <- "local_getis_ord_gstar"

  metric_summarizer(
    metric_nm = metric_nm,
    metric_fn = ww_local_getis_ord_g_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    na_rm = na_rm,
    metric_fn_options = list(
      wt = wt,
      alternative = alternative,
      nsim = nsim,
      ...
    )
  )
}

#' @rdname local_getis_ord_g
#' @export
ww_local_getis_ord_g_vec <- function(truth,
                                     estimate,
                                     wt = NULL,
                                     alternative = "two.sided",
                                     nsim = 499,
                                     na_rm = TRUE,
                                     ...,
                                     include_self = FALSE) {

  if (!inherits(wt, "listw")) {
    rlang::abort(
      "`wt` must be a 'listw' object",
      "i" = "You can create 'listw' objects using `build_weights()`"
    )
  }

  ww_local_getis_ord_g_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    as.vector(
      spdep::localG(
        x = resid,
        listw = wt,
        alternative = alternative,
        ...
      )
    )

  }

  metric_vec_template(
    metric_impl = ww_local_getis_ord_g_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

#' @rdname local_getis_ord_g
#' @export
ww_local_getis_ord_pvalue <- function(data, ...) {
  UseMethod("ww_local_getis_ord_pvalue")
}

ww_local_getis_ord_pvalue <- new_numeric_metric(ww_local_getis_ord_pvalue, "minimize")

#' @export
ww_local_getis_ord_pvalue.data.frame <- function(data,
                                                 truth,
                                                 estimate,
                                                 wt = NULL,
                                                 alternative = "two.sided",
                                                 nsim = 499,
                                                 na_rm = TRUE,
                                                 ...,
                                                 include_self = FALSE) {

  if (is.null(wt)) {
    wt <- ww_build_weights(data, include_self = include_self)
  }
  if (rlang::is_function(wt)) {
    wt <- do.call(wt, list(data))
  }
  metric_nm <- "local_getis_ord_g_pvalue"
  if (identical(attr(wt$neighbours, "self.included"), TRUE)) metric_nm <- "local_getis_ord_gstar_pvalue"

  metric_summarizer(
    metric_nm = metric_nm,
    metric_fn = ww_local_getis_ord_pvalue_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    na_rm = na_rm,
    metric_fn_options = list(
      wt = wt,
      alternative = alternative,
      nsim = nsim,
      ...
    )
  )
}

#' @rdname local_getis_ord_g
#' @export
ww_local_getis_ord_pvalue_vec <- function(truth,
                                          estimate,
                                          wt = NULL,
                                          alternative = "two.sided",
                                          nsim = 499,
                                          na_rm = TRUE,
                                          ...,
                                          include_self = FALSE) {

  if (!inherits(wt, "listw")) {
    rlang::abort(
      "`wt` must be a 'listw' object",
      "i" = "You can create 'listw' objects using `build_weights()`"
    )
  }

  ww_local_getis_ord_pvalue_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    out <- spdep::localG_perm(
      x = resid,
      listw = wt,
      alternative = alternative,
      nsim = nsim
    )
    out <- attr(out, "internals")
    as.vector(out[, 4])

  }

  metric_vec_template(
    metric_impl = ww_local_getis_ord_pvalue_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

#' @rdname local_getis_ord_g
#' @export
ww_local_getis_ord <- function(data,
                               truth,
                               estimate,
                               wt = NULL,
                               alternative = "two.sided",
                               nsim = 499,
                               na_rm = TRUE,
                               ...,
                               include_self = FALSE) {

  if (is.null(wt)) {
    wt <- ww_build_weights(data, include_self = include_self)
  }
  if (rlang::is_function(wt)) {
    wt <- do.call(wt, list(data))
  }

  metrics <- metric_set(ww_local_getis_ord_g, ww_local_getis_ord_pvalue)
  metrics(
    data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    wt = wt,
    alternative = alternative,
    nsim = nsim,
    na_rm = na_rm,
    ...,
    include_self = include_self
  )
}
