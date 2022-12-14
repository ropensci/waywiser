#' Local Moran's I statistic
#'
#' Calculate the local Moran's I statistic for model residuals.
#' `ww_local_moran_i()` returns the statistic itself, while
#' `ww_local_moran_pvalue()` returns the associated p value.
#'
#' @inheritParams yardstick::rmse
#' @inheritParams spdep::localmoran
#' @inheritParams ww_area_of_applicability
#' @param wt A "listw" object, for instance as created with [ww_build_weights()].
#' @param ... Additional arguments passed to [spdep::localmoran()].
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
#' ww_local_moran_i(guerry, Crm_prs, predictions)
#' ww_local_moran(guerry, Crm_prs, predictions)
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
                                        na_action = na.fail,
                                        ...) {
  spatial_yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    wt = wt,
    na_action = na_action,
    name = "local_moran_i",
    ...
  )
}

#' @rdname local_moran_i
#' @export
ww_local_moran_i_vec <- function(truth, estimate, wt, na_action = na.fail, ...) {
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
    na_action = na_action,
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
                                             na_action = na.fail,
                                             ...) {
  spatial_yardstick_df(
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    wt = wt,
    name = "local_moran_pvalue",
    na_action = na_action,
    ...
  )
}

#' @rdname local_moran_i
#' @export
ww_local_moran_pvalue_vec <- function(truth,
                                      estimate,
                                      wt = NULL,
                                      na_action = na.fail,
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
    na_action = na_action,
    impl = ww_local_moran_pvalue_impl,
    ...
  )
}
