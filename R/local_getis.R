#' Local Getis-Ord G and G* statistic
#'
#' Calculate the local Getis-Ord G and G* statistic for model residuals.
#'
#' @inheritParams yardstick::rmse
#' @inheritParams sfdep::local_g
#' @param seed A random seed to use for all metric calculations.
#'  Defaults to the current seed.
#'
#' @return
#' A tibble with columns .metric, .estimator, and .estimate and `nrow(data)` rows of values.
#' For grouped data frames, the number of rows returned will be the same as the number of groups.
#' For ww_local_getis_ord_g_vec(), a numeric vector of `length(truth)` (or NA).
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
#' ww_local_getis_ord_g(guerry_modeled, crime_pers, predictions, ctg, wts)
#' ww_local_g(guerry_modeled, crime_pers, predictions, ctg, wts)
#'
#' ww_local_getis_ord_g_star(guerry_modeled, crime_pers, predictions, ctg, wts)
#' ww_local_g_star(guerry_modeled, crime_pers, predictions, ctg, wts)
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
                                            nb,
                                            wt,
                                            alternative = "two.sided",
                                            nsim = 499,
                                            na_rm = TRUE,
                                            seed = NULL,
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
    metric_nm = "local_getis_ord_g",
    metric_fn = ww_local_getis_ord_g_vec,
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

#' @rdname local_getis_ord_g
#' @export
ww_local_getis_ord_g_vec <- function(truth,
                                     estimate,
                                     nb,
                                     wt,
                                     alternative = "two.sided",
                                     nsim = 499,
                                     na_rm = TRUE,
                                     seed = NULL,
                                     ...) {

  ww_local_getis_ord_g_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    if (!is.null(seed)) {
      old_seed <- .Random.seed
      set.seed(seed)
    }

    out <- as.vector(
      sfdep::local_g_perm(
        x = resid,
        nb = nb,
        wt = wt,
        alternative = alternative,
        nsim = nsim,
        ...
      )$gi
    )

    if (!is.null(seed)) {
      set.seed(old_seed)
    }

    out

  }

  metric_vec_template(
    metric_impl = ww_local_getis_ord_g_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
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
ww_local_getis_ord_g_pvalue.data.frame <- function(data, truth, estimate, nb, wt, alternative = "two.sided", nsim = 499, na_rm = TRUE, seed = NULL, ...) {

  metric_summarizer(
    metric_nm = "local_getis_ord_g_pvalue",
    metric_fn = ww_local_getis_ord_g_pvalue_vec,
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

#' @rdname local_getis_ord_g
#' @export
ww_local_getis_ord_g_pvalue_vec <- function(truth, estimate, nb, wt, alternative = "two.sided", nsim = 499, na_rm = TRUE, seed = NULL, ...) {

  ww_local_getis_ord_g_pvalue_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    if (!is.null(seed)) {
      old_seed <- .Random.seed
      set.seed(seed)
    }

    out <- as.vector(
      sfdep::local_g_perm(
        x = resid,
        nb = nb,
        wt = wt,
        alternative = alternative,
        nsim = nsim,
        ...
      )$p_value
    )

    if (!is.null(seed)) {
      set.seed(old_seed)
    }

    out

  }

  metric_vec_template(
    metric_impl = ww_local_getis_ord_g_pvalue_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

#' @rdname local_getis_ord_g
#' @export
ww_local_g <- function(data,
                       truth,
                       estimate,
                       nb,
                       wt,
                       alternative = "two.sided",
                       nsim = 499,
                       na_rm = TRUE,
                       seed = NULL,
                       ...) {
  metrics <- metric_set(ww_local_getis_ord_g, ww_local_getis_ord_g_pvalue)
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

#' @rdname local_getis_ord_g
#' @export
ww_local_getis_ord_g_star <- function(data, ...) {
  UseMethod("ww_local_getis_ord_g_star")
}

ww_local_getis_ord_g_star <- new_numeric_metric(ww_local_getis_ord_g_star, direction = "zero")

#' @export
ww_local_getis_ord_g_star.data.frame <- function(data,
                                            truth,
                                            estimate,
                                            nb,
                                            wt,
                                            alternative = "two.sided",
                                            nsim = 499,
                                            na_rm = TRUE,
                                            seed = NULL,
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
    metric_nm = "local_getis_ord_g_star",
    metric_fn = ww_local_getis_ord_g_star_vec,
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

#' @rdname local_getis_ord_g
#' @export
ww_local_getis_ord_g_star_vec <- function(truth,
                                     estimate,
                                     nb,
                                     wt,
                                     alternative = "two.sided",
                                     nsim = 499,
                                     na_rm = TRUE,
                                     seed = NULL,
                                     ...) {

  ww_local_getis_ord_g_star_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    old_seed <- .Random.seed
    set.seed(seed)

    out <- as.vector(
      sfdep::local_gstar_perm(
        x = resid,
        nb = nb,
        wt = wt,
        alternative = alternative,
        nsim = nsim,
        ...
      )$gi
    )

    set.seed(old_seed)

    out

  }

  metric_vec_template(
    metric_impl = ww_local_getis_ord_g_star_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

#' @rdname local_getis_ord_g
#' @export
ww_local_getis_ord_g_star_pvalue <- function(data, ...) {
  UseMethod("ww_local_getis_ord_g_star_pvalue")
}

ww_local_getis_ord_g_star_pvalue <- new_numeric_metric(ww_local_getis_ord_g_star_pvalue, "minimize")

#' @export
ww_local_getis_ord_g_star_pvalue.data.frame <- function(data, truth, estimate, nb, wt, alternative = "two.sided", nsim = 499, na_rm = TRUE, seed = NULL, ...) {

  metric_summarizer(
    metric_nm = "local_getis_ord_g_star_pvalue",
    metric_fn = ww_local_getis_ord_g_star_pvalue_vec,
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

#' @rdname local_getis_ord_g
#' @export
ww_local_getis_ord_g_star_pvalue_vec <- function(truth, estimate, nb, wt, alternative = "two.sided", nsim = 499, na_rm = TRUE, seed = NULL, ...) {

  ww_local_getis_ord_g_star_pvalue_impl <- function(truth, estimate, ...) {
    resid <- truth - estimate

    old_seed <- .Random.seed
    set.seed(seed)

    out <- as.vector(
      sfdep::local_gstar_perm(
        x = resid,
        nb = nb,
        wt = wt,
        alternative = alternative,
        nsim = nsim,
        ...
      )$p_value
    )

    set.seed(old_seed)

    out

  }

  metric_vec_template(
    metric_impl = ww_local_getis_ord_g_star_pvalue_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

#' @rdname local_getis_ord_g
#' @export
ww_local_g_star <- function(data,
                       truth,
                       estimate,
                       nb,
                       wt,
                       alternative = "two.sided",
                       nsim = 499,
                       na_rm = TRUE,
                       seed = NULL,
                       ...) {
  metrics <- metric_set(ww_local_getis_ord_g_star, ww_local_getis_ord_g_star_pvalue)
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
