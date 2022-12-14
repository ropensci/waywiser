#' Local Getis-Ord G and G* statistic
#'
#' Calculate the local Getis-Ord G and G* statistic for model residuals.
#' `ww_local_getis_ord_g()` returns the statistic itself, while
#' `ww_local_getis_ord_pvalue()` returns the associated p value.
#'
#' @srrstats {G1.4} roxygen2 documentation
#' @srrstats {G2.7} This function relies on yardstick and dplyr and therefore only handles data.frame and vector input.
#' @srrstats {G2.8} Method dispatch enforces data.frame inputs
#' @srrstats {G2.10} Column extraction is properly handled within yardstick.
#' @srrstats {G2.14} Any function may be passed to na_action
#' @srrstats {G2.14a} Any function may be passed to na_action
#' @srrstats {G2.14b} Any function may be passed to na_action
#' @srrstats {G2.14c} Any function may be passed to na_action
#' @srrstats {G2.15} Any function may be passed to na_action
#' @srrstats {G2.16} Any function may be passed to na_action
#'
#' @inheritParams ww_global_geary_c
#' @inheritParams spdep::localG_perm
#' @param ... Arguments passed to [spdep::localG_perm()]
#'
#' @return
#' A tibble with columns .metric, .estimator, and .estimate and `nrow(data)` rows of values.
#' For grouped data frames, the number of rows returned will be the same as the number of groups.
#' For `_vec()` functions, a numeric vector of `length(truth)` (or NA).
#'
#' @family autocorrelation metrics
#' @family yardstick metrics
#'
#' @examples
#' guerry_lm <- lm(Crm_prs ~ Litercy, guerry)
#' guerry$predictions <- predict(guerry_lm, guerry)
#'
#' ww_local_getis_ord_g(guerry, Crm_prs, predictions)
#' ww_local_getis_ord_g_pvalue(guerry, Crm_prs, predictions)
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
                                            na_action = na.fail,
                                            ...) {
  spatial_yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    wt = wt,
    na_action = na_action,
    name = "local_getis_ord_g",
    ...
  )
}

#' @rdname local_getis_ord_g
#' @export
ww_local_getis_ord_g_vec <- function(truth, estimate, wt, na_action = na.fail, ...) {
  ww_local_getis_ord_g_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate
    as.vector(
      spdep::localG(
        x = resid,
        listw = wt,
        ...
      )
    )

  }
  spatial_yardstick_vec(
    truth = truth,
    estimate = estimate,
    wt = wt,
    na_action = na_action,
    impl = ww_local_getis_ord_g_impl,
    ...
  )
}

#' @rdname local_getis_ord_g
#' @export
ww_local_getis_ord_g_pvalue <- function(data, ...) {
  UseMethod("ww_local_getis_ord_g_pvalue")
}

ww_local_getis_ord_g_pvalue <- new_numeric_metric(ww_local_getis_ord_g_pvalue, "minimize")

#' @export
ww_local_getis_ord_g_pvalue.data.frame <- function(data,
                                                 truth,
                                                 estimate,
                                                 wt = NULL,
                                                 na_action = na.fail,
                                                 ...) {
  spatial_yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    wt = wt,
    na_action = na_action,
    name = "local_getis_ord_g_pvalue",
    ...
  )
}

#' @rdname local_getis_ord_g
#' @export
ww_local_getis_ord_g_pvalue_vec <- function(truth, estimate, wt, na_action = na.fail, ...) {
  ww_local_getis_ord_pvalue_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate
    out <- spdep::localG_perm(
      x = resid,
      listw = wt,
      ...
    )
    out <- attr(out, "internals")
    as.vector(out[, 4])
  }

  spatial_yardstick_vec(
    truth = truth,
    estimate = estimate,
    wt = wt,
    na_action = na_action,
    impl = ww_local_getis_ord_pvalue_impl,
    ...
  )
}
