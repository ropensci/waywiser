#' Local Moran's I statistic
#'
#' Calculate the local Moran's I statistic for model residuals.
#'
#' @inheritParams yardstick::rmse
#' @inheritParams sfdep::local_c_perm
#' @param seed A random seed to use for all metric calculations.
#'  Defaults to the current seed.
#'
#' @return
#' A tibble with columns .metric, .estimator, and .estimate and `nrow(data)` rows of values.
#' For grouped data frames, the number of rows returned will be the same as the number of groups.
#' For ww_local_geary_c_vec(), a numeric vector of `length(truth)` (or NA).
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
#' ww_local_geary_c(guerry_modeled, crime_pers, predictions, ctg, wts)
#' ww_local_geary(guerry_modeled, crime_pers, predictions, ctg, wts)
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
                                        nb,
                                        wt,
                                        alternative = "greater",
                                        nsim = 499,
                                        na_rm = TRUE,
                                        seed = .Random.seed,
                                        ...) {

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
    metric_nm = "local_geary_c",
    metric_fn = ww_local_geary_c_vec,
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

#' @rdname local_geary_c
#' @export
ww_local_geary_c_vec <- function(truth,
                                 estimate,
                                 nb,
                                 wt,
                                 na_rm = TRUE,
                                 ...) {

  ww_local_geary_c_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    sfdep::local_c(
      x = resid,
      nb = nb,
      wt = wt,
      ...
    )

  }

  metric_vec_template(
    metric_impl = ww_local_geary_c_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
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
ww_local_geary_pvalue.data.frame <- function(data, truth, estimate, nb, wt, alternative = "greater", nsim = 499, na_rm = TRUE, seed = .Random.seed, ...) {

  metric_summarizer(
    metric_nm = "local_geary_pvalue",
    metric_fn = ww_local_geary_pvalue_vec,
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

#' @rdname local_geary_c
#' @export
ww_local_geary_pvalue_vec <- function(truth, estimate, nb, wt, alternative = "greater", nsim = 499, na_rm = TRUE, seed = .Random.seed, ...) {

  ww_local_geary_pvalue_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    old_seed <- .Random.seed
    set.seed(seed)

    out <- sfdep::local_c_perm(
      x = resid,
      nb = nb,
      wt = wt,
      alternative = alternative,
      nsim = nsim,
      ...
    )$p_ci

    set.seed(old_seed)

    out

  }

  metric_vec_template(
    metric_impl = ww_local_geary_pvalue_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

#' @rdname local_geary_c
#' @export
ww_local_geary <- function(data,
                           truth,
                           estimate,
                           nb,
                           wt,
                           alternative = "greater",
                           nsim = 499,
                           na_rm = TRUE,
                           seed = .Random.seed,
                           ...) {
  metrics <- metric_set(ww_local_geary_c, ww_local_geary_pvalue)
  metrics(
    data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    nb = nb,
    wt = wt,
    alternative = alternative,
    nsim = nsim,
    na_rm = na_rm,
    seed = seed,
    ...
  )
}
