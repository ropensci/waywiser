#' Local Moran's I statistic
#'
#' Calculate the local Moran's I statistic for model residuals.
#'
#' @inheritParams yardstick::rmse
#' @inheritParams sfdep::local_moran
#'
#' @return
#' A tibble with columns .metric, .estimator, and .estimate and `nrow(data)` rows of values.
#' For grouped data frames, the number of rows returned will be the same as the number of groups.
#' For ww_local_moran_i_vec(), a numeric vector of `length(truth)` (or NA).
#'
#' @rdname local_moran_i
#' @export
ww_local_moran_i <- function(data, ...) {
  UseMethod("ww_local_moran_i")
}

#' @rdname local_moran_i
#' @export
ww_local_moran_i.data.frame <- function(data,
                                        truth,
                                        estimate,
                                        nb,
                                        wt,
                                        alternative = "two.sided",
                                        nsim = 499,
                                        na_rm = TRUE,
                                        seed = .Random.seed,
                                        ...) {

  metric_summarizer(
    metric_nm = "local_moran_i",
    metric_fn = ww_local_moran_i_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    na_rm = na_rm,
    metric_fn_options = list(
      nb = nb,
      wt = wt,
      alternative = alternative,
      nsim = nsim,
      seed = seed
    ),
    ...
  )
}

ww_local_moran_i <- new_numeric_metric(ww_local_moran_i, direction = "zero")

#' @rdname local_moran_i
#' @export
ww_local_moran_i_vec <- function(truth,
                                 estimate,
                                 nb,
                                 wt,
                                 alternative = "two.sided",
                                 nsim = 499,
                                 na_rm = TRUE,
                                 seed = .Random.seed,
                                 ...) {

  ww_local_moran_i_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    old_seed <- .Random.seed
    set.seed(seed)

    out <- sfdep::local_moran(
      x = resid,
      nb = nb,
      wt = wt,
      alternative = alternative,
      nsim = nsim,
      ...
    )$ii

    set.seed(old_seed)

    out

  }

  metric_vec_template(
    metric_impl = ww_local_moran_i_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

#' @rdname local_moran_i
#' @export
ww_local_moran_pvalue <- function(data, ...) {
  UseMethod("ww_local_moran_pvalue")
}

ww_local_moran_pvalue <- new_numeric_metric(ww_local_moran_pvalue, "minimize")

#' @rdname local_moran_i
#' @export
ww_local_moran_pvalue.data.frame <- function(data, truth, estimate, nb, wt, alternative = "two.sided", nsim = 499, na_rm = TRUE, seed = .Random.seed, ...) {

  metric_summarizer(
    metric_nm = "local_moran_pvalue",
    metric_fn = ww_local_moran_pvalue_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    na_rm = na_rm,
    metric_fn_options = list(
      nb = nb,
      wt = wt,
      alternative = alternative,
      nsim = nsim
    ),
    ...
  )
}

#' @rdname local_moran_i
#' @export
ww_local_moran_pvalue_vec <- function(truth, estimate, nb, wt, alternative = "two.sided", nsim = 499, na_rm = TRUE, seed = .Random.seed, ...) {

  ww_local_moran_pvalue_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    old_seed <- .Random.seed
    set.seed(seed)

    out <- sfdep::local_moran(
      x = resid,
      nb = nb,
      wt = wt,
      alternative = alternative,
      nsim = nsim,
      ...
    )$p_ii

    set.seed(old_seed)

    out

  }

  metric_vec_template(
    metric_impl = ww_local_moran_pvalue_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

#' @rdname local_moran_i
#' @export
ww_local_moran <- function(data,
                           truth,
                           estimate,
                           nb,
                           wt,
                           alternative = "greater",
                           randomization = TRUE,
                           na_rm = TRUE,
                           seed = .Random.seed,
                           ...) {
  metrics <- metric_set(ww_local_moran_i, ww_local_moran_pvalue)
  metrics(
    data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    nb = nb,
    wt = wt,
    alternative = alternative,
    randomization = randomization,
    na_rm = na_rm,
    seed = seed,
    ...
  )
}
