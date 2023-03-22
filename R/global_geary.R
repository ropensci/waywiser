#' Global Geary's C statistic
#'
#' Calculate the global Geary's C statistic for model residuals.
#' `ww_global_geary_c()` returns the statistic itself, while
#' `ww_global_geary_pvalue()` returns the associated p value.
#' These functions are meant to help assess model predictions, for instance by
#' identifying if there are clusters of higher residuals than expected. For
#' statistical testing and inference applications, use
#' [spdep::geary.test()] instead.
#'
#' These functions can be used for geographic or projected coordinate reference
#' systems and expect 2D data.
#'
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
                                         na_rm = FALSE,
                                         ...) {
  spatial_yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    wt = wt,
    na_rm = na_rm,
    name = "global_geary_c",
    ...
  )
}

#' @rdname global_geary_c
#' @export
ww_global_geary_c_vec <- function(truth, estimate, wt, na_rm = FALSE, ...) {
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
    na_rm = na_rm,
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
                                              na_rm = FALSE,
                                              ...) {
  spatial_yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    wt = wt,
    name = "global_geary_pvalue",
    na_rm = na_rm,
    ...
  )
}

#' @rdname global_geary_c
#' @export
ww_global_geary_pvalue_vec <- function(truth,
                                       estimate,
                                       wt = NULL,
                                       na_rm = FALSE,
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
    na_rm = na_rm,
    impl = ww_global_geary_pvalue_impl,
    ...
  )
}
