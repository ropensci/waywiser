#' Global Geary's C statistic
#'
#' Calculate the global Geary's C statistic for model residuals.
#' `ww_global_geary_c()` returns the statistic itself, while
#' `ww_global_geary_pvalue()` returns the associated p value.
#'
#' These functions can be used for geographic or projected coordinate reference
#' systems and expect 2D data.
#'
#' @srrstats {SP1.0} Domain of applicability specified above.
#' @srrstats {SP1.1} Dimensional domain of applicability specified above.
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
#' @srrstats {SP2.6} Input type requirements are documented.
#' @srrstats {SP3.0} Users are given total control over weights.
#' @srrstats {SP3.0a} Users are given total control over weights.
#' @srrstats {SP3.0b} Users are given total control over weights.
#' @srrstats {SP3.1} Users are given total control over weights.
#' @inheritParams yardstick::rmse
#' @inheritParams spdep::geary.test
#' @inheritParams ww_area_of_applicability
#' @param wt A `listw` object, for instance as created with [ww_build_weights()].
#' For data.frame input, may also be a function that takes `data` and returns a
#' `listw` object.
#' @param ... Additional arguments passed to [spdep::geary()] (for
#' `ww_global_geary_c()`) or [spdep::geary.test()] (for
#' `ww_global_geary_pvalue()`).
#'
#' @family autocorrelation metrics
#' @family yardstick metrics
#'
#' @srrstats {SP4.0} Return values are of a unique format
#' @srrstats {SP4.0b} Return values are of a unique format
#' @srrstats {SP4.2} Returns are explicitly documented
#'
#' @return
#' A tibble with columns .metric, .estimator, and .estimate and 1 row of values.
#' For grouped data frames, the number of rows returned will be the same as the
#' number of groups.
#' For `_vec()` functions, a single value (or NA).
#'
#' @examples
#' guerry_model <- guerry
#' guerry_lm <- lm(Crm_prs ~ Litercy, guerry_model)
#' guerry_model$predictions <- predict(guerry_lm, guerry_model)
#'
#' ww_global_geary_c(guerry_model, Crm_prs, predictions)
#' ww_global_geary_pvalue(guerry_model, Crm_prs, predictions)
#'
#' wt <- ww_build_weights(guerry_model)
#'
#' ww_global_geary_c_vec(
#'   guerry_model$Crm_prs,
#'   guerry_model$predictions,
#'   wt = wt
#' )
#' ww_global_geary_pvalue_vec(
#'   guerry_model$Crm_prs,
#'   guerry_model$predictions,
#'   wt = wt
#' )
#'
#' @references
#' Geary, R. C. (1954). "The Contiguity Ratio and Statistical Mapping". The
#' Incorporated Statistician. 5 (3): 115–145. doi:10.2307/2986645.
#'
#' Cliff, A. D., Ord, J. K. 1981 Spatial processes, Pion, p. 17.
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
                                         na_action = na.fail,
                                         ...) {
  spatial_yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    wt = wt,
    na_action = na_action,
    name = "global_geary_c",
    ...
  )
}

#' @rdname global_geary_c
#' @export
ww_global_geary_c_vec <- function(truth, estimate, wt, na_action = na.fail, ...) {
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

  spatial_yardstick_vec(
    truth = truth,
    estimate = estimate,
    wt = wt,
    na_action = na_action,
    impl = ww_global_geary_c_impl,
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
                                              na_action = na.fail,
                                              ...) {
  spatial_yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    wt = wt,
    name = "global_geary_pvalue",
    na_action = na_action,
    ...
  )
}

#' @rdname global_geary_c
#' @export
ww_global_geary_pvalue_vec <- function(truth,
                                       estimate,
                                       wt = NULL,
                                       na_action = na.fail,
                                       ...) {

  ww_global_geary_pvalue_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate
    if (all(resid == 0)) {
      return(NA_real_)
    }

    spdep::geary.test(
      x = resid,
      listw = wt,
      ...
    )$p.value
  }

  spatial_yardstick_vec(
    truth = truth,
    estimate = estimate,
    wt = wt,
    na_action = na_action,
    impl = ww_global_geary_pvalue_impl,
    ...
  )

}
