#' Global Geary's C statistic
#'
#' Calculate the global Geary's C statistic for model residuals.
#' `ww_global_geary_c()` returns the statistic itself, while
#' `ww_global_geary_pvalue()` returns the associated p value.
#'
#' @inheritParams yardstick::rmse
#' @inheritParams spdep::geary.test
#' @inheritParams ww_area_of_applicability
#' @param wt A "listw" object, for instance as created with [ww_build_weights()].
#' @param randomization variance of I calculated under the assumption of randomisation, if FALSE normality
#' @param ... Additional arguments passed to [spdep::geary.test()].
#'
#' @family autocorrelation metrics
#' @family yardstick metrics
#'
#' @return
#' A tibble with columns .metric, .estimator, and .estimate and `nrow(data)` rows of values.
#' For grouped data frames, the number of rows returned will be the same as the number of groups.
#' For `_vec()` functions, a single value (or NA).
#'
#' @examples
#'
#' guerry_lm <- lm(Crm_prs ~ Litercy, guerry)
#' guerry$predictions <- predict(guerry_lm, guerry)
#'
#' \dontrun{
#' ww_global_geary(guerry, Crm_prs, predictions)
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
