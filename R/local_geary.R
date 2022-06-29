#' Local Geary's C statistic
#'
#' Calculate the local Geary's C statistic for model residuals.
#' `ww_local_geary_c()` returns the statistic itself, while
#' `ww_local_geary_pvalue()` returns the associated p value.
#' `ww_local_geary()` returns both.
#'
#' @inheritParams yardstick::rmse
#' @inheritParams spdep::localC_perm
#' @param wt A "listw" object, for instance as created with [ww_build_weights()].
#' @param ... Additional arguments passed to [spdep::localC_perm()].
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
#' ww_local_geary_c(guerry_modeled, crime_pers, predictions)
#' ww_local_geary(guerry_modeled, crime_pers, predictions)
#'
#' @rdname local_geary_c
#' @export
ww_local_geary_c <- function(data, ...) {
  UseMethod("ww_local_geary_c")
}

ww_local_geary_c <- new_numeric_metric(ww_local_geary_c, direction = "zero")

#' @export
ww_local_geary_c.data.frame <- function(data,
                                        truth,
                                        estimate,
                                        wt = NULL,
                                        na_rm = TRUE,
                                        ...) {

  if (is.null(wt)) {
    wt <- ww_build_weights(data)
  }
  if (rlang::is_function(wt)) {
    wt <- do.call(wt, list(data))
  }

  metric_summarizer(
    metric_nm = "local_geary_c",
    metric_fn = ww_local_geary_c_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    na_rm = na_rm,
    metric_fn_options = list(
      wt = wt,
      ...
    )
  )
}

#' @rdname local_geary_c
#' @export
ww_local_geary_c_vec <- function(truth,
                                 estimate,
                                 wt,
                                 na_rm = TRUE,
                                 ...) {

  if (!inherits(wt, "listw")) {
    rlang::abort(
      "`wt` must be a 'listw' object",
      "i" = "You can create 'listw' objects using `build_weights()`"
    )
  }

  dots <- list(...)
  dots <- dots$zero.policy

  ww_local_geary_c_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    spdep::localC(
      x = resid,
      listw = wt,
      zero.policy = dots
    )

  }

  metric_vec_template(
    metric_impl = ww_local_geary_c_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

#' @rdname local_geary_c
#' @export
ww_local_geary_pvalue <- function(data, ...) {
  UseMethod("ww_local_geary_pvalue")
}

ww_local_geary_pvalue <- new_numeric_metric(ww_local_geary_pvalue, "minimize")

#' @export
ww_local_geary_pvalue.data.frame <- function(data,
                                             truth,
                                             estimate,
                                             wt = NULL,
                                             alternative = "two.sided",
                                             nsim = 499,
                                             na_rm = TRUE,
                                             ...) {

  if (is.null(wt)) {
    wt <- ww_build_weights(data)
  }
  if (rlang::is_function(wt)) {
    wt <- do.call(wt, list(data))
  }

  metric_summarizer(
    metric_nm = "local_geary_pvalue",
    metric_fn = ww_local_geary_pvalue_vec,
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

#' @rdname local_geary_c
#' @export
ww_local_geary_pvalue_vec <- function(truth,
                                      estimate,
                                      wt = NULL,
                                      alternative = "two.sided",
                                      nsim = 499,
                                      na_rm = TRUE,
                                      ...) {

  if (!inherits(wt, "listw")) {
    rlang::abort(
      "`wt` must be a 'listw' object",
      "i" = "You can create 'listw' objects using `build_weights()`"
    )
  }

  ww_local_geary_pvalue_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    out <- spdep::localC_perm(
      x = resid,
      listw = wt,
      alternative = alternative,
      nsim = nsim,
      ...
    )

    as.vector(
      attr(out, "pseudo-p")[, 4]
    )
  }

  metric_vec_template(
    metric_impl = ww_local_geary_pvalue_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

#' @rdname local_geary_c
#' @export
ww_local_geary <- function(data,
                           truth,
                           estimate,
                           wt = NULL,
                           alternative = "two.sided",
                           nsim = 499,
                           na_rm = TRUE,
                           ...) {

  if (is.null(wt)) {
    wt <- ww_build_weights(data)
  }
  if (rlang::is_function(wt)) {
    wt <- do.call(wt, list(data))
  }

  metrics <- metric_set(ww_local_geary_c, ww_local_geary_pvalue)
  metrics(
    data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    wt = wt,
    alternative = alternative,
    nsim = nsim,
    na_rm = na_rm,
    ...
  )
}
