#' Local Getis-Ord G and G* statistic
#'
#' Calculate the local Getis-Ord G and G* statistic for model residuals.
#' `ww_local_getis_ord_g()` returns the statistic itself, while
#' `ww_local_getis_ord_pvalue()` returns the associated p value.
#' These functions are meant to help assess model predictions, for instance by
#' identifying clusters of higher residuals than expected. For statistical
#' testing and inference applications, use [spdep::localG_perm()] instead.
#'
#' These functions can be used for geographic or projected coordinate reference
#' systems and expect 2D data.
#'
#' @inheritParams ww_global_geary_c
#' @inheritParams spdep::localG_perm
#' @param ... Additional arguments passed to [spdep::localG()] (for
#' `ww_local_getis_ord_g()`) or [spdep::localG_perm()] (for
#' `ww_local_getis_ord_pvalue()`).
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
#' ww_local_getis_ord_g(guerry_model, Crm_prs, predictions)
#' ww_local_getis_ord_g_pvalue(guerry_model, Crm_prs, predictions)
#'
#' wt <- ww_build_weights(guerry_model)
#'
#' ww_local_getis_ord_g_vec(
#'   guerry_model$Crm_prs,
#'   guerry_model$predictions,
#'   wt = wt
#' )
#' ww_local_getis_ord_g_pvalue_vec(
#'   guerry_model$Crm_prs,
#'   guerry_model$predictions,
#'   wt = wt
#' )
#'
#' @references
#' Ord, J. K. and Getis, A. 1995. Local spatial autocorrelation statistics:
#' distributional issues and an application. Geographical Analysis, 27, 286–306.
#' doi: 10.1111/j.1538-4632.1995.tb00912.x
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
                                            na_rm = FALSE,
                                            ...) {
  spatial_yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    wt = wt,
    na_rm = na_rm,
    name = "local_getis_ord_g",
    ...
  )
}

#' @rdname local_getis_ord_g
#' @export
ww_local_getis_ord_g_vec <- function(truth, estimate, wt, na_rm = FALSE, ...) {
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
    na_rm = na_rm,
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
                                                   na_rm = FALSE,
                                                   ...) {
  spatial_yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    wt = wt,
    na_rm = na_rm,
    name = "local_getis_ord_g_pvalue",
    ...
  )
}

#' @rdname local_getis_ord_g
#' @export
ww_local_getis_ord_g_pvalue_vec <- function(truth, estimate, wt, na_rm = FALSE, ...) {
  ww_local_getis_ord_pvalue_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate
    out <- spdep::localG_perm(
      x = resid,
      listw = wt,
      ...
    )
    as.vector(attr(out, "internals")[, "Pr(z != E(Gi))"])
  }

  spatial_yardstick_vec(
    truth = truth,
    estimate = estimate,
    wt = wt,
    na_rm = na_rm,
    impl = ww_local_getis_ord_pvalue_impl,
    ...
  )
}
