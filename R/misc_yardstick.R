yardstick_df <- function(data, truth, estimate, na_action, name, metric_fun, ..., na_rm = FALSE) {
  if (missing(metric_fun)) metric_fun <- get(paste0("ww_", name, "_vec"))
  metric_summarizer(
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
}

spatial_yardstick_df <- function(data, truth, estimate, wt, na_action, name, ..., na_rm = FALSE) {
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
    data = data,
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

yardstick_vec <- function(truth, estimate, na_action, impl, wt = NULL, ..., na_rm = FALSE) {

  if (!is.vector(truth)) rlang::abort("`truth` must be a numeric vector.")
  if (!is.vector(estimate)) rlang::abort("`estimate` must be a numeric vector.")

  if (!is.numeric(truth)) rlang::abort("`truth` must be numeric.")
  if (!is.numeric(estimate)) rlang::abort("`estimate` must be numeric.")

  if (length(truth) == 0) rlang::abort("0 values were passed to `truth`.")
  if (length(estimate) == 0) rlang::abort("0 values were passed to `estimate`.")

  if (length(truth) != length(estimate)) {
    rlang::abort(
      glue::glue("Length of `truth` ({length(truth)}) and `estimate` ({length(estimate)}) must match.")
    )
  }

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

spatial_yardstick_vec <- function(truth, estimate, wt, na_action, impl, ..., na_rm = FALSE) {
  if (!inherits(wt, "listw")) {
    rlang::abort(
      "`wt` must be a 'listw' object",
      "i" = "You can create 'listw' objects using `build_weights()`"
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
