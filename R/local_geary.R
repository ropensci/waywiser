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
#' @inheritParams ww_global_geary_c
#' @inheritParams spdep::localC_perm
#' @param ... Additional arguments passed to [spdep::localC()] (for
#' `ww_local_geary_c()`) or [spdep::localC_perm()] (for
#' `ww_local_geary_pvalue()`).
#'
#' @srrstats {SP4.0} Return values are of a unique format
#' @srrstats {SP4.0b} Return values are of a unique format
#' @srrstats {SP4.2} Returns are explicitly documented
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
