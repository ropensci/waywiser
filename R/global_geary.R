#' Global Moran's I statistic
#'
#' Calculate the global Moran's I statistic for model residuals.
#' `ww_global_geary_c()` returns the statistic itself, while
#' `ww_global_geary_pvalue()` returns the associated p value.
#' `ww_global_geary()` returns both.
#'
#' @inheritParams yardstick::rmse
#' @inheritParams sfdep::global_c
#' @inheritParams sfdep::global_c_perm
#'
#' @return
#' A tibble with columns .metric, .estimator, and .estimate.
#' For grouped data frames, the number of rows returned will be the same as the number of groups times the number of metrics.
#' For ww_global_geary_c_vec(), a single numeric value (or NA).
#'
#' @examples
#' data(guerry, package = "sfdep")
#'
#' guerry_modeled <- guerry
#' guerry_lm <- lm(crime_pers ~ literacy, guerry_modeled)
#' guerry_modeled$predictions <- predict(guerry_lm, guerry_modeled)
#'
#' ctg <- st_contiguity(guerry)
#' wts <- st_weights(ctg)
#'
#' ww_global_geary_c(guerry_modeled, crime_pers, predictions, ctg, wts)
#' ww_global_geary(guerry_modeled, crime_pers, predictions, ctg, wts)
#'
#' @rdname global_geary_c
#' @export
ww_global_geary_c <- function(data, ...) {
  UseMethod("ww_global_geary_c")
}

ww_global_geary_c <- new_numeric_metric(ww_global_geary_c, direction = "zero")

#' @export
ww_global_geary_c.data.frame <- function(data, truth, estimate, nb, wt, allow_zero = TRUE, na_rm = TRUE, ...) {

  if (rlang::is_function(nb)) {
    nb <- do.call(nb, data)
  }
  if (rlang::is_function(wt)) {
    wt <- do.call(wt, data)
  }
  if (is.null(wt)) {
    wt <- sfdep::st_weights(nb)
  }

  metric_summarizer(
    metric_nm = "global_geary_c",
    metric_fn = ww_global_geary_c_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    na_rm = na_rm,
    metric_fn_options = list(
      nb = nb,
      wt = wt
    ),
    ...
  )
}

#' @rdname global_geary_c
#' @export
ww_global_geary_c_vec <- function(truth, estimate, nb, wt, allow_zero = TRUE, na_rm = TRUE, ...) {

  ww_global_geary_c_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    sfdep::global_c(
      x = resid,
      nb = nb,
      wt = wt,
      allow_zero = TRUE,
      ...
    )$C
  }

  metric_vec_template(
    metric_impl = ww_global_geary_c_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
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
ww_global_geary_pvalue.data.frame <- function(data, truth, estimate, nb, wt, alternative = "greater", nsim = 499, allow_zero = NULL, na_rm = TRUE, ...) {

  if (rlang::is_function(nb)) {
    nb <- do.call(nb, data)
  }
  if (rlang::is_function(wt)) {
    wt <- do.call(wt, data)
  }
  if (is.null(wt)) {
    wt <- sfdep::st_weights(nb)
  }

  metric_summarizer(
    metric_nm = "global_geary_pvalue",
    metric_fn = ww_global_geary_pvalue_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    na_rm = na_rm,
    metric_fn_options = list(
      nb = nb,
      wt = wt,
      alternative = alternative,
      nsim = nsim,
      allow_zero = allow_zero
    ),
    ...
  )
}

#' @rdname global_geary_c
#' @export
ww_global_geary_pvalue_vec <- function(truth, estimate, nb, wt, alternative = "greater", nsim = 499, allow_zero = NULL, na_rm = TRUE, ...) {

  ww_global_geary_pvalue_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    sfdep::global_c_perm(
      x = resid,
      nb = nb,
      wt = wt,
      alternative = alternative,
      nsim = nsim,
      allow_zero = allow_zero,
      ...
    )$p.value
  }

  metric_vec_template(
    metric_impl = ww_global_geary_pvalue_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

#' @rdname global_geary_c
#' @export
ww_global_geary <- function(data,
                            truth,
                            estimate,
                            nb,
                            wt,
                            alternative = "greater",
                            nsim = 499,
                            allow_zero = NULL,
                            na_rm = TRUE,
                            ...) {
  metrics <- metric_set(ww_global_geary_c, ww_global_geary_pvalue)
  metrics(
    data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    nb = nb,
    wt = wt,
    alternative = alternative,
    nsim = nsim,
    allow_zero = allow_zero,
    na_rm = na_rm,
    ...
  )
}
