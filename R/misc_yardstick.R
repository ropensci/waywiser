#' Workhorse function handling yardstick metrics for the package
#'
#' @param name The human-understandable name of the metric, to return in the
#' output data frame.
#' @param metric_fun The name of the function to use to calculate the metric.
#' @param na_rm Here for compatibility with yardstick; ignored.
#' @inheritParams yardstick::rmse
#' @inheritParams ww_area_of_applicability
#' @inheritParams rlang::args_dots_empty
#' @inheritParams rlang::args_error_context
#'
#' @return A tibble with one row and three columns: `.metric`, containing `name`,
#' `.estimator`, containing `standard`, and `.estimate`, the metric estimate.
#' sf objects may also have a `geometry` column with the unioned geometry of
#' inputs.
#'
#' @noRd
yardstick_df <- function(data, truth, estimate, na_rm, name, metric_fun, ..., case_weights = NULL,
                         error_call = rlang::caller_env()) {
  if (missing(metric_fun)) metric_fun <- get(paste0("ww_", name, "_vec"))
  out <- metric_reframer(
    name = name,
    fn = metric_fun,
    data = data,
    na_rm = na_rm,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    fn_options = list(
      ...
    ),
    error_call = error_call
  )

  if (inherits(out, "sf")) {
    sf::st_geometry(out) <- NULL
  }

  out
}

metric_reframer <- function(name, fn, data, truth, estimate, ..., na_rm = TRUE, fn_options = list(), error_call = rlang::caller_env()) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)
  truth <- ww_eval_select(expr = truth, data = data, error_call = error_call)
  estimate <- ww_eval_select(expr = estimate, data = data, error_call = error_call)

  group_rows <- dplyr::group_rows(data)
  group_keys <- dplyr::group_keys(data)
  data <- dplyr::ungroup(data)
  groups <- vctrs::vec_chop(data, indices = group_rows)
  out <- vector("list", length = length(groups))

  for (i in seq_along(groups)) {
    group <- groups[[i]]

    group_truth <- group[[truth]]
    group_estimate <- group[[estimate]]

    elt_out <- list(
      .metric = name,
      .estimator = "standard",
      .estimate = as.vector(
        rlang::inject(
          withCallingHandlers(
            fn(
              truth = group_truth,
              estimate = group_estimate,
              na_rm = na_rm,
              !!!fn_options
            ),
            error = function(cnd) {
              cnd$call <- error_call
              rlang::cnd_signal(cnd)
            }
          )
        )
      )
    )

    elt_out <- vctrs::vec_recycle_common(!!!elt_out)
    out[[i]] <- tibble::new_tibble(elt_out)
  }

  group_keys <- vctrs::vec_rep_each(group_keys, times = vctrs::list_sizes(out))
  out <- vctrs::vec_rbind(!!!out)
  out <- vctrs::vec_cbind(group_keys, out)

  out
}

# cribbed from yardstick 1.2.0
ww_eval_select <- function(expr, data, arg, ..., error_call = rlang::caller_env()) {
  rlang::check_dots_empty()
  out <- tidyselect::eval_select(
    expr = expr,
    data = data,
    allow_predicates = FALSE,
    allow_rename = FALSE,
    allow_empty = FALSE,
    error_call = error_call
  )
  out <- names(out)
  if (length(out) != 1L) {
    message <- paste0("`", arg, "` must select exactly 1 column from `data`.")
    rlang::abort(message, call = error_call)
  }
  out
}

#' Workhorse function handling spatial yardstick metrics for the package
#'
#' @inheritParams ww_global_geary_c
#' @inheritParams yardstick::rmse
#' @inheritParams ww_area_of_applicability
#' @inheritParams rlang::args_dots_empty
#' @inheritParams rlang::args_error_context
#' @inheritParams yardstick_df
#'
#' @return A tibble with one row and three columns: `.metric`, containing `name`,
#' `.estimator`, containing `standard`, and `.estimate`, the metric estimate.
#' sf objects may also have a `geometry` column with the unioned geometry of
#' inputs.
#'
#' @noRd
spatial_yardstick_df <- function(data, truth, estimate, wt, na_rm, name, ..., case_weights = NULL,
                                 error_call = rlang::caller_env()) {
  if (is.null(wt)) {
    wt <- ww_build_weights(data)
  }
  if (rlang::is_function(wt)) {
    wt <- do.call(wt, list(data))
  }

  truth <- enquo(truth)
  estimate <- enquo(estimate)
  truth <- ww_eval_select(expr = truth, data = data, error_call = rlang::caller_env())
  estimate <- ww_eval_select(expr = estimate, data = data, error_call = rlang::caller_env())

  if (yardstick_any_missing(data[[truth]], data[[estimate]], NULL)) {
    rlang::abort(
      c(
        "Missing values in data.",
        i = "waywiser can't handle missing data for functions that use spatial weights."
      )
    )
  }

  metric_fun <- get(paste0("ww_", name, "_vec"))

  if (grepl("getis_ord_g", name) &&
    identical(attr(wt$neighbours, "self.included"), TRUE)) {
    name <- gsub("ord_g", "ord_gstar", name)
  }

  yardstick_df(
    data = as.data.frame(data),
    truth = {{ truth }},
    estimate = {{ estimate }},
    na_rm = na_rm,
    name = name,
    metric_fun = metric_fun,
    wt = wt,
    ...,
    error_call = error_call
  )
}

#' Workhorse function powering yardstick metrics
#'
#' @inheritParams ww_global_geary_c
#' @inheritParams yardstick::rmse
#' @param impl The metric implementation function.
#'
#' @return A vector of metric estimates
#'
#' @noRd
yardstick_vec <- function(truth, estimate, na_rm, impl, wt = NULL, ..., case_weights = NULL) {
  if (!is.vector(truth)) rlang::abort("`truth` must be a numeric vector.")
  if (!is.vector(estimate)) rlang::abort("`estimate` must be a numeric vector.")

  if (!is.numeric(truth)) rlang::abort("`truth` must be numeric.")
  if (!is.numeric(estimate)) rlang::abort("`estimate` must be numeric.")

  if (length(truth) != length(estimate)) {
    rlang::abort(
      glue::glue("Length of `truth` ({length(truth)}) and `estimate` ({length(estimate)}) must match.")
    )
  }

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, NULL)
    truth <- result$truth
    estimate <- result$estimate
  } else if (yardstick_any_missing(truth, estimate, NULL)) {
    return(NA_real_)
  }

  if (length(truth) == 0) rlang::abort("0 non-missing values were passed to `truth`.")
  if (length(estimate) == 0) rlang::abort("0 non-missing values were passed to `estimate`.")

  impl(truth = truth, estimate = estimate, ...)
}

#' Workhorse function powering spatial yardstick metrics
#'
#' @inheritParams yardstick_vec
#' @inheritParams spatial_yardstick_df
#'
#' @return A vector of metric estimates
#'
#' @noRd
spatial_yardstick_vec <- function(truth, estimate, wt, na_rm = TRUE, impl, ..., case_weights = NULL) {
  if (!inherits(wt, "listw")) {
    rlang::abort(
      c(
        "`wt` must be a 'listw' object",
        "i" = "You can create 'listw' objects using `ww_build_weights()`"
      )
    )
  }

  if (yardstick_any_missing(truth, estimate, NULL)) {
    rlang::abort(
      c(
        "Missing values in data.",
        i = "waywiser can't handle missing data for functions that use spatial weights."
      )
    )
  }

  yardstick_vec(
    truth,
    estimate,
    na_rm,
    impl,
    wt = wt,
    ...
  )
}
