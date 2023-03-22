#' Local Geary's C statistic
#'
#' Calculate the local Geary's C statistic for model residuals.
#' `ww_local_geary_c()` returns the statistic itself, while
#' `ww_local_geary_pvalue()` returns the associated p value.
#' These functions are meant to help assess model predictions, for instance by
#' identifying clusters of higher residuals than expected. For statistical
#' testing and inference applications, use [spdep::localC_perm()] instead.
#'
#' These functions can be used for geographic or projected coordinate reference
#' systems and expect 2D data.
#'
#' @inheritParams ww_global_geary_c
#' @inheritParams spdep::localC_perm
#' @param ... Additional arguments passed to [spdep::localC()] (for
#' `ww_local_geary_c()`) or [spdep::localC_perm()] (for
#' `ww_local_geary_pvalue()`).
#'
#' @return
#' A tibble with columns .metric, .estimator, and .estimate and `nrow(data)`
#' rows of values.
#' For `_vec()` functions, a numeric vector of `length(truth)` (or NA).
#'
#' @family autocorrelation metrics
#' @family yardstick metrics
#'
#' @examples
#' guerry_model <- guerry
#' guerry_lm <- lm(Crm_prs ~ Litercy, guerry_model)
#' guerry_model$predictions <- predict(guerry_lm, guerry_model)
#'
#' ww_local_geary_c(guerry_model, Crm_prs, predictions)
#' ww_local_geary_pvalue(guerry_model, Crm_prs, predictions)
#'
#' wt <- ww_build_weights(guerry_model)
#'
#' ww_local_geary_c_vec(
#'   guerry_model$Crm_prs,
#'   guerry_model$predictions,
#'   wt = wt
#' )
#' ww_local_geary_pvalue_vec(
#'   guerry_model$Crm_prs,
#'   guerry_model$predictions,
#'   wt = wt
#' )
#'
#' @references
#' Anselin, L. 1995. Local indicators of spatial association, Geographical
#' Analysis, 27, pp 93–115. doi: 10.1111/j.1538-4632.1995.tb00338.x.
#'
#' Anselin, L. 2019. A Local Indicator of Multivariate Spatial Association:
#' Extending Geary's C. Geographical Analysis, 51, pp 133-150.
#' doi: 10.1111/gean.12164
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
                                        na_rm = FALSE,
                                        ...) {
  spatial_yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    wt = wt,
    na_rm = na_rm,
    name = "local_geary_c",
    ...
  )
}

#' @rdname local_geary_c
#' @export
ww_local_geary_c_vec <- function(truth, estimate, wt, na_rm = FALSE, ...) {
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
    na_rm = na_rm,
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
                                             na_rm = FALSE,
                                             ...) {
  spatial_yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    wt = wt,
    name = "local_geary_pvalue",
    na_rm = na_rm,
    ...
  )
}

#' @rdname local_geary_c
#' @export
ww_local_geary_pvalue_vec <- function(truth,
                                      estimate,
                                      wt = NULL,
                                      na_rm = FALSE,
                                      ...) {
  ww_local_geary_pvalue_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    out <- spdep::localC_perm(
      x = resid,
      listw = wt,
      ...
    )

    as.vector(
      attr(out, "pseudo-p")[, "Pr(z != E(Ci))"]
    )
  }
  spatial_yardstick_vec(
    truth = truth,
    estimate = estimate,
    wt = wt,
    na_rm = na_rm,
    impl = ww_local_geary_pvalue_impl,
    ...
  )
}
