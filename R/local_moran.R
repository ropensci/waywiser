#' Local Moran's I statistic
#'
#' Calculate the local Moran's I statistic for model residuals.
#' `ww_local_moran_i()` returns the statistic itself, while
#' `ww_local_moran_pvalue()` returns the associated p value.
#' These functions are meant to help assess model predictions, for instance by
#' identifying clusters of higher residuals than expected. For statistical
#' testing and inference applications, use [spdep::localmoran_perm()] instead.
#'
#' These functions can be used for geographic or projected coordinate reference
#' systems and expect 2D data.
#'
#' @inheritParams ww_global_geary_c
#' @inheritParams spdep::localmoran
#' @param ... Additional arguments passed to [spdep::localmoran()].
#'
#' @inherit ww_local_geary_c return
#'
#' @family autocorrelation metrics
#' @family yardstick metrics
#'
#' @examples
#' guerry_model <- guerry
#' guerry_lm <- lm(Crm_prs ~ Litercy, guerry_model)
#' guerry_model$predictions <- predict(guerry_lm, guerry_model)
#'
#' ww_local_moran_i(guerry_model, Crm_prs, predictions)
#' ww_local_moran_pvalue(guerry_model, Crm_prs, predictions)
#'
#' wt <- ww_build_weights(guerry_model)
#'
#' ww_local_moran_i_vec(
#'   guerry_model$Crm_prs,
#'   guerry_model$predictions,
#'   wt = wt
#' )
#' ww_local_moran_pvalue_vec(
#'   guerry_model$Crm_prs,
#'   guerry_model$predictions,
#'   wt = wt
#' )
#'
#' @references
#' Anselin, L. 1995. Local indicators of spatial association, Geographical
#' Analysis, 27, pp 93–115. doi: 10.1111/j.1538-4632.1995.tb00338.x.
#'
#' Sokal, R. R, Oden, N. L. and Thomson, B. A. 1998. Local Spatial
#' Autocorrelation in a Biological Model. Geographical Analysis, 30, pp 331–354.
#' doi: 10.1111/j.1538-4632.1998.tb00406.x
#'
#' @rdname local_moran_i
#' @export
ww_local_moran_i <- function(data, ...) {
  UseMethod("ww_local_moran_i")
}

ww_local_moran_i <- new_numeric_metric(ww_local_moran_i, direction = "zero")

#' @export
ww_local_moran_i.data.frame <- function(data,
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
    name = "local_moran_i",
    ...
  )
}

#' @rdname local_moran_i
#' @export
ww_local_moran_i_vec <- function(truth, estimate, wt, na_rm = FALSE, ...) {
  ww_local_moran_i_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    spdep::localmoran(
      x = resid,
      listw = wt,
      ...
    )[, 1]
  }
  spatial_yardstick_vec(
    truth = truth,
    estimate = estimate,
    wt = wt,
    na_rm = na_rm,
    impl = ww_local_moran_i_impl,
    ...
  )
}

#' @rdname local_moran_i
#' @export
ww_local_moran_pvalue <- function(data, ...) {
  UseMethod("ww_local_moran_pvalue")
}

ww_local_moran_pvalue <- new_numeric_metric(ww_local_moran_pvalue, "minimize")

#' @export
ww_local_moran_pvalue.data.frame <- function(data,
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
    name = "local_moran_pvalue",
    na_rm = na_rm,
    ...
  )
}

#' @rdname local_moran_i
#' @export
ww_local_moran_pvalue_vec <- function(truth,
                                      estimate,
                                      wt = NULL,
                                      na_rm = FALSE,
                                      ...) {
  ww_local_moran_pvalue_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    spdep::localmoran(
      x = resid,
      listw = wt,
      ...
    )[, 5]
  }
  spatial_yardstick_vec(
    truth = truth,
    estimate = estimate,
    wt = wt,
    na_rm = na_rm,
    impl = ww_local_moran_pvalue_impl,
    ...
  )
}
