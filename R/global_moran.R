#' Global Moran's I statistic
#'
#' Calculate the global Moran's I statistic for model residuals.
#' `ww_global_moran_i()` returns the statistic itself, while
#' `ww_global_moran_pvalue()` returns the associated p value.
#' `ww_global_moran()` returns both.
#'
#' @inheritParams yardstick::rmse
#' @inheritParams sfdep::global_moran
#'
#' @return
#' A tibble with columns .metric, .estimator, and .estimate.
#' For grouped data frames, the number of rows returned will be the same as the number of groups times the number of metrics.
#' For ww_global_moran_i_vec(), a single numeric value (or NA).
#'
#' @rdname global_moran_i
#' @export
ww_global_moran_i <- function(data, ...) {
  UseMethod("ww_global_moran_i")
}

ww_global_moran_i <- new_numeric_metric(ww_global_moran_i, direction = "zero")

#' @rdname local_moran_i
#' @export
ww_global_moran_i.data.frame <- function(data, truth, estimate, nb, wt, na_rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "global_moran_i",
    metric_fn = ww_global_moran_i_vec,
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

#' @rdname global_moran_i
#' @export
ww_global_moran_i_vec <- function(truth, estimate, nb, wt, na_rm = TRUE, ...) {

  ww_global_moran_i_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    sfdep::global_moran(
      x = resid,
      nb = nb,
      wt = wt,
      na_ok = na_rm,
      ...
    )$I
  }

  metric_vec_template(
    metric_impl = ww_global_moran_i_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

#' @rdname global_moran_i
#' @export
ww_global_moran_pvalue <- function(data, ...) {
  UseMethod("ww_global_moran_pvalue")
}

ww_global_moran_pvalue <- new_numeric_metric(ww_global_moran_pvalue, "minimize")

#' @rdname local_moran_i
#' @export
ww_global_moran_pvalue.data.frame <- function(data, truth, estimate, nb, wt, alternative = "greater", randomization = TRUE, na_rm = TRUE, ...) {

  metric_summarizer(
    metric_nm = "global_moran_pvalue",
    metric_fn = ww_global_moran_pvalue_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    na_rm = na_rm,
    metric_fn_options = list(
      nb = nb,
      wt = wt,
      alternative = alternative,
      randomization = randomization
    ),
    ...
  )
}

#' @rdname global_moran_i
#' @export
ww_global_moran_pvalue_vec <- function(truth, estimate, nb, wt, alternative, randomization, na_rm = TRUE, ...) {

  ww_global_moran_pvalue_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    sfdep::global_moran_test(
      x = resid,
      nb = nb,
      wt = wt,
      alternative = alternative,
      randomization = randomization,
      ...
    )$p.value
  }

  metric_vec_template(
    metric_impl = ww_global_moran_pvalue_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

#' @rdname global_moran_i
#' @export
ww_global_moran <- function(data,
                            truth,
                            estimate,
                            nb,
                            wt,
                            alternative = "greater",
                            randomization = TRUE,
                            na_rm = TRUE,
                            ...) {
  metrics <- metric_set(ww_global_moran_i, ww_global_moran_pvalue)
  metrics(
    data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    nb = nb,
    wt = wt,
    alternative = alternative,
    randomization = randomization,
    na_rm = na_rm,
    ...
  )
}
