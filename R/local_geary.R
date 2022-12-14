#' Local Geary's C statistic
#'
#' Calculate the local Geary's C statistic for model residuals.
#' `ww_local_geary_c()` returns the statistic itself, while
#' `ww_local_geary_pvalue()` returns the associated p value.
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
#' @inheritParams spdep::localC_perm
#' @param ... Additional arguments passed to [spdep::localC_perm()].
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
#' ww_local_geary_c(guerry, Crm_prs, predictions)
#' ww_local_geary_pvalue(guerry, Crm_prs, predictions)
#'
#' @rdname local_geary_c
#' @export
ww_local_geary_c <- function(data, ...) {
  UseMethod("ww_local_geary_c")
}

ww_local_geary_c <- new_numeric_metric(ww_local_geary_c, direction = "zero")

#' @export
ww_local_geary_c.data.frame  <- function(data,
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
    name = "local_geary_c",
    ...
  )
}

#' @rdname local_geary_c
#' @export
ww_local_geary_c_vec <- function(truth, estimate, wt, na_action = na.fail, ...) {

  ww_local_geary_c_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    spdep::localC(
      x = resid,
      listw = wt,
      ...
    )
  }
  spatial_yardstick_vec(
    truth = truth,
    estimate = estimate,
    wt = wt,
    na_action = na_action,
    impl = ww_local_geary_c_impl,
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
                                             na_action = na.fail,
                                             ...) {
  spatial_yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    wt = wt,
    name = "local_geary_pvalue",
    na_action = na_action,
    ...
  )
}

#' @rdname local_geary_c
#' @export
ww_local_geary_pvalue_vec <- function(truth,
                                      estimate,
                                      wt = NULL,
                                      na_action = na.fail,
                                      ...) {
  ww_local_geary_pvalue_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    out <- spdep::localC_perm(
      x = resid,
      listw = wt,
      ...
    )

    as.vector(
      attr(out, "pseudo-p")[, 4]
    )
  }
  spatial_yardstick_vec(
    truth = truth,
    estimate = estimate,
    wt = wt,
    na_action = na_action,
    impl = ww_local_geary_pvalue_impl,
    ...
  )
}
