#' Workhorse function handling yardstick metrics for the package
#'
#' @param name The human-understandable name of the metric, to return in the
#' output data frame.
#' @param metric_fun The name of the function to use to calculate the metric.
#' @param na_rm Here for compatibility with yardstick; ignored.
#' @inheritParams yardstick::rmse
#' @inheritParams ww_area_of_applicability
#' @inheritParams rlang::args_dots_empty
#'
#' @srrstats {G1.4a} Documented internal functions
#'
#' @return A tibble with one row and three columns: `.metric`, containing `name`,
#' `.estimator`, containing `standard`, and `.estimate`, the metric estimate.
#' sf objects may also have a `geometry` column with the unioned geometry of
#' inputs.
#'
#' @noRd
#' @srrstats {G2.7} Due to relying on yardstick and dplyr, these functions only accept dataframes
#' @srrstats {G2.8} Above enforced by method dispatch
yardstick_df <- function(data, truth, estimate, na_action, name, metric_fun, ..., na_rm = FALSE) {

  #' @srrstats {SP2.7} Standardized validation routine for yardstick metrics
  #' @srrstats {SP2.8} Pre-processing routine for yardstick metrics
  #' @srrstats {SP2.9} Retains relevant metadata

  if (missing(metric_fun)) metric_fun <- get(paste0("ww_", name, "_vec"))
  #' @srrstats {G2.10} Extraction handled by yardstick:
  out <- metric_summarizer(
    metric_nm = name,
    metric_fn = metric_fun,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    na_rm = FALSE,
    metric_fn_options = list(
      na_action = na_action,
      ...
    )
  )

  if (inherits(out, "sf")) {
    sf::st_geometry(out) <- NULL
  }

  out
}

#' Workhorse function handling spatial yardstick metrics for the package
#'
#' @inheritParams ww_global_geary_c
#' @inheritParams yardstick::rmse
#' @inheritParams ww_area_of_applicability
#' @inheritParams rlang::args_dots_empty
#' @inheritParams yardstick_df
#'
#' @srrstats {G1.4a} Documented internal functions
#'
#' @return A tibble with one row and three columns: `.metric`, containing `name`,
#' `.estimator`, containing `standard`, and `.estimate`, the metric estimate.
#' sf objects may also have a `geometry` column with the unioned geometry of
#' inputs.
#'
#' @noRd
#' @srrstats {G2.7} Due to relying on yardstick and dplyr, these functions only accept dataframes
#' @srrstats {G2.8} Above enforced by method dispatch
spatial_yardstick_df <- function(data, truth, estimate, wt, na_action, name, ..., na_rm = FALSE) {

  #' @srrstats {SP2.7} Standardized validation routine for yardstick metrics
  #' @srrstats {SP2.8} Pre-processing routine for yardstick metrics
  #' @srrstats {SP2.9} Retains relevant metadata

  #' @srrstats {SP3.0} Total user control over neighborhoods:
  if (is.null(wt)) {
    wt <- ww_build_weights(data)
  }
  if (rlang::is_function(wt)) {
    wt <- do.call(wt, list(data))
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
    na_action = na_action,
    name = name,
    metric_fun = metric_fun,
    wt = wt,
    ...,
    na_rm = FALSE
  )
}

#' Workhorse function powering yardstick metrics
#'
#' @inheritParams ww_global_geary_c
#' @inheritParams yardstick::rmse
#' @param impl The metric implementation function.
#' @param na_rm Ignored; set `na_action` instead. Here for compatibility with
#' [yardstick::metric_set()], but not accessible via exported functions.
#' @srrstats {G1.4a} Documented internal functions
#'
#' @return A vector of metric estimates
#'
#' @noRd
yardstick_vec <- function(truth, estimate, na_action, impl, wt = NULL, ..., na_rm = FALSE) {

  #' @srrstats {SP2.7} Standardized validation routine for yardstick metrics
  #' @srrstats {SP2.8} Pre-processing routine for yardstick metrics
  #' @srrstats {SP2.9} Retains relevant metadata

  #' @srrstats {G2.8} Ensuring all processing gets standardized inputs
  #' @srrstats {G2.1} Assertions on types of inputs
  #' @srrstats {G2.1a} Secondary documentation on input types
  if (!is.vector(truth)) rlang::abort("`truth` must be a numeric vector.")
  if (!is.vector(estimate)) rlang::abort("`estimate` must be a numeric vector.")

  #' @srrstats {G2.1} Assertions on types of inputs
  #' @srrstats {G2.12} List columns produce errors
  if (!is.numeric(truth)) rlang::abort("`truth` must be numeric.")
  if (!is.numeric(estimate)) rlang::abort("`estimate` must be numeric.")

  #' @srrstats {G2.0} Assertions on lengths of inputs
  #' @srrstats {G2.0a} Secondary documentation on input lengths
  if (length(truth) == 0) rlang::abort("0 values were passed to `truth`.")
  if (length(estimate) == 0) rlang::abort("0 values were passed to `estimate`.")

  if (length(truth) != length(estimate)) {
    rlang::abort(
      glue::glue("Length of `truth` ({length(truth)}) and `estimate` ({length(estimate)}) must match.")
    )
  }

  #' @srrstats {G2.13} Checking for missing data
  processed_truth <- check_for_missing(
    truth,
    na_action,
    "true values",
    "(`truth`)"
  )
  processed_estimate <- check_for_missing(
    estimate,
    na_action,
    "estimated values",
    "(`estimate`)"
  )

  if (length(processed_truth) != length(processed_estimate)) {
    was_na <- is.na(truth) | is.na(estimate)
    truth[was_na] <- NA
    estimate[was_na] <- NA

    processed_truth <- check_for_missing(
      truth,
      na_action,
      "true values",
      "(`truth`)"
    )
    processed_estimate <- check_for_missing(
      estimate,
      na_action,
      "estimated values",
      "(`estimate`)"
    )
  }

  if (length(processed_truth) != length(processed_estimate)) {
    rlang::abort(
      "`truth` and `estimate` were not the same length after running `na_action`."
    )
  }

  if (!is.null(wt) && length(wt$neighbours) != length(truth)) {
    rlang::abort(
      "`truth` and `estimate` were not the same length as `wt$neighbours` after running `na_action`."
    )
  }

  # We don't pass wt here because of scary inheritance reasons;
  # don't try
  metric_vec_template(
    metric_impl = impl,
    truth = processed_truth,
    estimate = processed_estimate,
    cls = "numeric",
    na_rm = FALSE,
    ...
  )

}

#' Workhorse function powering spatial yardstick metrics
#'
#' @inheritParams yardstick_vec
#' @inheritParams spatial_yardstick_df
#' @srrstats {G1.4a} Documented internal functions
#'
#' @return A vector of metric estimates
#'
#' @noRd
spatial_yardstick_vec <- function(truth, estimate, wt, na_action, impl, ..., na_rm = FALSE) {
  #' @srrstats {SP2.7} Standardized validation routine for yardstick metrics
  #' @srrstats {SP2.8} Pre-processing routine for yardstick metrics
  #' @srrstats {SP2.9} Retains relevant metadata
  if (!inherits(wt, "listw")) {
    rlang::abort(
      c(
        "`wt` must be a 'listw' object",
        "i" = "You can create 'listw' objects using `ww_build_weights()`"
      )
    )
  }

  yardstick_vec(
    truth,
    estimate,
    na_action,
    impl,
    wt = wt,
    ...,
    na_rm = FALSE
  )
}
