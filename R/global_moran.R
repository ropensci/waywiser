#' Global Moran's I statistic
#'
#' Calculate the global Moran's I statistic for model residuals.
#' `ww_global_moran_i()` returns the statistic itself, while
#' `ww_global_moran_pvalue()` returns the associated p value.
#'
#' @inheritParams yardstick::rmse
#' @inheritParams spdep::moran.test
#' @inheritParams ww_area_of_applicability
#' @param wt A "listw" object, for instance as created with [ww_build_weights()].
#' @param randomization variance of I calculated under the assumption of randomisation, if FALSE normality
#' @param ... Additional arguments passed to [spdep::moran.test()].
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
#' guerry_lm <- lm(Crm_prs ~ Litercy, guerry)
#' guerry$predictions <- predict(guerry_lm, guerry)
#'
#' ww_global_moran_i(guerry, Crm_prs, predictions)
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
                                         na_action = na.fail,
                                         ...) {
  spatial_yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    wt = wt,
    na_action = na_action,
    name = "global_moran_i",
    ...
  )
}

#' @rdname global_moran_i
#' @export
ww_global_moran_i_vec <- function(truth,
                                  estimate,
                                  wt = NULL,
                                  na_action = na.fail,
                                  ...) {
  ww_global_moran_i_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate
    spdep::moran(
      x = resid,
      listw = wt,
      n = length(wt$neighbours),
      S0 = spdep::Szero(wt),
      ...
    )$I

  }
  spatial_yardstick_vec(
    truth = truth,
    estimate = estimate,
    wt = wt,
    na_action = na_action,
    impl = ww_global_moran_i_impl,
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
                                              na_action = na.fail,
                                              ...) {
  spatial_yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    wt = wt,
    na_action = na_action,
    name = "global_moran_pvalue",
    ...
  )
}

#' @rdname global_moran_i
#' @export
ww_global_moran_pvalue_vec <- function(truth,
                                       estimate,
                                       wt = NULL,
                                       na_action = na.fail,
                                       ...) {
  ww_global_moran_pvalue_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate
    if (all(resid == 0)) {
      return(NA_real_)
    }

    spdep::moran.test(
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
    impl = ww_global_moran_pvalue_impl,
    ...
  )
}
