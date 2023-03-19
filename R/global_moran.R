#' Global Moran's I statistic
#'
#' Calculate the global Moran's I statistic for model residuals.
#' `ww_global_moran_i()` returns the statistic itself, while
#' `ww_global_moran_pvalue()` returns the associated p value.
#' These functions are meant to help assess model predictions, for instance by
#' identifying if there are clusters of higher residuals than expected. For
#' statistical testing and inference applications, use
#' [spdep::moran.test()] instead.
#'
#' These functions can be used for geographic or projected coordinate reference
#' systems and expect 2D data.
#'
#' @inheritParams ww_global_geary_c
#' @inheritParams spdep::moran.test
#' @param ... Additional arguments passed to [spdep::moran()] (for
#' `ww_global_moran_i()`) or [spdep::moran.test()] (for
#' `ww_global_moran_pvalue()`).
#'
#' @family autocorrelation metrics
#' @family yardstick metrics
#'
#' @inherit ww_global_geary_c return
#'
#' @examples
#' guerry_model <- guerry
#' guerry_lm <- lm(Crm_prs ~ Litercy, guerry_model)
#' guerry_model$predictions <- predict(guerry_lm, guerry_model)
#'
#' ww_global_moran_i(guerry_model, Crm_prs, predictions)
#' ww_global_moran_pvalue(guerry_model, Crm_prs, predictions)
#'
#' wt <- ww_build_weights(guerry_model)
#'
#' ww_global_moran_i_vec(
#'   guerry_model$Crm_prs,
#'   guerry_model$predictions,
#'   wt = wt
#' )
#' ww_global_moran_pvalue_vec(
#'   guerry_model$Crm_prs,
#'   guerry_model$predictions,
#'   wt = wt
#' )
#'
#' @references
#' Moran, P.A.P. (1950). "Notes on Continuous Stochastic Phenomena." Biometrika,
#' 37(1/2), pp 17. doi: 10.2307/2332142
#'
#' Cliff, A. D., Ord, J. K. 1981 Spatial processes, Pion, p. 17.
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
                                         na_rm = FALSE,
                                         ...) {
  spatial_yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    wt = wt,
    na_rm = na_rm,
    name = "global_moran_i",
    ...
  )
}

#' @rdname global_moran_i
#' @export
ww_global_moran_i_vec <- function(truth,
                                  estimate,
                                  wt = NULL,
                                  na_rm = FALSE,
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
    na_rm = na_rm,
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
                                              na_rm = FALSE,
                                              ...) {
  spatial_yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    wt = wt,
    na_rm = na_rm,
    name = "global_moran_pvalue",
    ...
  )
}

#' @rdname global_moran_i
#' @export
ww_global_moran_pvalue_vec <- function(truth,
                                       estimate,
                                       wt = NULL,
                                       na_rm = FALSE,
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
    na_rm = na_rm,
    impl = ww_global_moran_pvalue_impl,
    ...
  )
}
