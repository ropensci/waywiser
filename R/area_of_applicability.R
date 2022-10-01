#' Find the area of applicability
#'
#' This function calculates the "area of applicability" of a model, as
#' introduced by Meyer and Pebesma (2021). While the initial paper introducing
#' this method focused on spatial models, there is nothing inherently spatial
#' about the method; it can be used with any type of data.
#'
#' Predictions made on points "inside" the area of applicability should be as
#' accurate as predictions made on the data provided to `y` or `validation`.
#' That means that generally `y` or `validation` should be your final hold-out
#' set, or the assessment set of a cross-validation fold, so that predictions
#' on points inside the area of applicability are accurately described by your
#' reported model metrics.
#'
#' @param x Either a data frame, matrix, formula
#' (specifying predictor terms on the right-hand side), recipe
#' (from [recipes::recipe()].
#'
#' If `x` is a recipe, it should be the same one used to pre-process the data
#' used in your model. If the recipe used to build the area of applicability
#' doesn't match the one used to build the model, the returned area of
#' applicability won't be correct.
#'
#' @param rset An `rset` object, produced by resampling functions from rsample
#' or spatialsample. When using this method, `x` must be a formula or recipe,
#' in which case the expected predictors will be extracted and any specified
#' pre-processing will be performed, or missing (either not provided, provided
#' as NULL, or provided as NA) in which case all provided variables will be
#' used to calculate the area of applicability.
#'
#' @param data The data frame representing your "training" data, when using the
#'  formula or recipe methods.
#'
#' @param validation A data frame or matrix containing the data used to
#'  validate your model. This should be the same data as used to calculate all
#'  model accuracy metrics.
#'
#'  If this argument is `NULL`, then this function will use the training data
#'  (from `x` or `data`) to calculate within-sample distances.
#'  This may result in the area of applicability threshold being set too high,
#'  with the result that too many points are classed as "inside" the area of
#'  applicability.
#'
#' @param importance Either:
#'
#'   * A data.frame with two columns: `term`, containing the names of each
#'     variable in the training and validation data, and `estimate`, containing
#'     the (raw or scaled) feature importance for each variable.
#'   * An object of class `vi` with at least two columns, `Variable` and `Importance`.
#'
#' All variables in the training data (`x` or `data`, depending on the context)
#' must have a matching importance estimate, and all terms with importance
#' estimates must be in the training data.
#'
#' @param ... Not currently used.
#'
#' @details
#'
#' This method assumes your model was fit using dummy variables in the place of
#' any non-numeric predictor, and that you have one importance score per
#' dummy variable. Having non-numeric predictors will cause this function to
#' fail.
#'
#' @return
#'
#' A `ww_area_of_applicability` object, which can be used with [predict()]
#' to calculate the distance of new data to the original training data, and
#' determine if new data is within a model's area of applicability.
#'
#' @family area of applicability functions
#'
#' @examplesIf rlang::is_installed("vip")
#' library(vip)
#' train <- gen_friedman(1000, seed = 101)  # ?vip::gen_friedman
#' test <- train[701:1000, ]
#' train <- train[1:700, ]
#' pp <- stats::ppr(y ~ ., data = train, nterms = 11)
#' importance <- vi_permute(
#'   pp,
#'   target = "y",
#'   metric = "rsquared",
#'   pred_wrapper = predict
#' )
#'
#' ww_area_of_applicability(y ~ ., train, test, importance = importance)
#'
#' @references
#' H. Meyer and E. Pebesma. 2021. "Predicting into unknown space? Estimating
#' the area of applicability of spatial prediction models," Methods in Ecology
#' and Evolution 12(9), pp 1620 - 1633, doi: 10.1111/2041-210X.13650.
#'
#' @export
ww_area_of_applicability <- function(x, ...) {
  UseMethod("ww_area_of_applicability")
}

#' @export
ww_area_of_applicability.default <- function(x, ...) {
  cls <- class(x)[1]
  rlang::abort(
    c(
      "`x` isn't a supported object type.",
      i = "`ww_area_of_applicability()` can only handle data.frames, matrices, formulas, and recipes.",
      x = glue::glue("`x` is a {cls}")
    ),
    call = rlang::caller_env()
  )
}

#' @export
#' @rdname ww_area_of_applicability
ww_area_of_applicability.data.frame <- function(x, validation = NULL, importance, ...) {
  rlang::check_dots_empty()
  training <- hardhat::mold(x, NA_real_)
  if (!is.null(validation)) validation <- hardhat::mold(validation, NA_real_)
  ww_area_of_applicability_impl(training, validation, importance, ...)
}

#' @export
#' @rdname ww_area_of_applicability
ww_area_of_applicability.matrix <- ww_area_of_applicability.data.frame

#' @export
#' @rdname ww_area_of_applicability
ww_area_of_applicability.formula <- function(x, data, validation = NULL, importance, ...) {
  rlang::check_dots_empty()
  training <- hardhat::mold(x, data)
  if (!is.null(validation)) validation <- hardhat::mold(x, validation)
  ww_area_of_applicability_impl(training, validation, importance, ...)
}

#' @export
#' @rdname ww_area_of_applicability
ww_area_of_applicability.recipe <- ww_area_of_applicability.formula

#' @export
#' @rdname ww_area_of_applicability
ww_area_of_applicability.rset <- function(rset, x, importance, ...) {
  rlang::check_dots_empty()

  if (missing(x) || identical(x, NULL) || identical(x, NA)) x <- NA_real_

  aoa_calcs <- purrr::map(
    rset$splits,
    function(rsplit) {
      training <- rsample::analysis(rsplit)
      validation <- rsample::assessment(rsplit)

      if (identical(x, NA_real_)) {
        training <- hardhat::mold(training, x)
        validation <- hardhat::mold(validation, x)
      } else {
        training <- hardhat::mold(x, training)
        validation <- hardhat::mold(x, validation)
      }
      ww_area_of_applicability_impl(
        training,
        validation,
        importance,
        include_di = TRUE
      )
    }
  )

  aoa <- aoa_calcs[[1]]
  aoa$training <- rset$splits[[1]]$data
  aoa$sds <- purrr::map_dbl(aoa$training, stats::sd, na.rm = TRUE)
  aoa$means <- purrr::map_dbl(aoa$training, mean, na.rm = TRUE)
  aoa$d_bar <- mean(unlist(purrr::map(aoa, purrr::chuck, "d_bar")))

  di <- unlist(purrr::map(aoa, purrr::chuck, "di"))
  aoa["di"] <- NULL
  aoa$aoa_threshold <- calc_aoa(di)

  aoa

}

tidy_importance <- function(importance, ...) {
  UseMethod("tidy_importance")
}

tidy_importance.vi <- function(importance, ...) {
  data.frame(
    term = importance[["Variable"]],
    estimate = importance[["Importance"]]
  )
}

tidy_importance.data.frame <- function(importance, ...) {
  if (!all(c("term", "estimate") %in% names(importance))) {
    rlang::abort(
      "'term' and 'estimate' must be columns in `importance`",
      call = rlang::caller_env()
    )
  }
  importance
}

tidy_importance.default <- function(importance, ...) {
  cls <- class(importance)[1]
  rlang::abort(
    glue::glue("Can't construct a tidy importance table from an object of class {cls}")
  )
}

ww_area_of_applicability_prep <- function(training, validation, importance) {
  training <- training$predictors

  validation <- check_di_validation(training, validation)

  importance <- check_di_importance(training, importance)
  check_di_columns_numeric(training, validation)

  list(
    training = training,
    validation = validation,
    importance = importance
  )
}

ww_area_of_applicability_impl <- function(training, validation, importance, ..., include_di = FALSE) {

  blueprint <- training$blueprint
  res <- ww_area_of_applicability_prep(training, validation, importance)

  # Comments reference section numbers from Meyer and Pebesma 2021
  # (doi: 10.1111/2041-210X.13650)

  # 2.1 Standardization of predictor variables
  # 2.2 Weighting of variables
  res <- standardize_and_weight(res$training, res$validation, res$importance)
  di <- calc_di(training, validation, importance)
  aoa_threshold <- calc_aoa(di$di)

  aoa <- hardhat::new_model(
    training = res$training,
    importance = res$importance,
    sds = res$sds,
    means = res$means,
    di = di$di,
    d_bar = di$d_bar,
    aoa_threshold = aoa_threshold,
    blueprint = blueprint,
    class = "ww_area_of_applicability"
  )

  if (!include_di) aoa["di"] <- NULL

  aoa
}

calc_di <- function(training, validation, importance) {
  res <- ww_area_of_applicability_prep(training, validation, importance)

  # 2.3 Multivariate distance calculation
  # Calculates the distance between each point in the `validation` set
  # (or `training`, if `validation` is `NULL`)
  # to the closest point in the training set
  dk <- calculate_dk(res$training, res$validation)

  # 2.4 Dissimilarity index
  # Find the mean nearest neighbor distance between training points:
  dists <- proxyC::dist(as.matrix(res$training))
  diag(dists) <- NA
  d_bar <- Matrix::mean(dists, na.rm = TRUE)

  list(
    # Use d_bar to rescale dk from 2.3
    di = dk / d_bar,
    d_bar = d_bar
  )

}

standardize_and_weight <- function(training, validation, importance) {
  # Store standard deviations and means of all predictors from training
  # We'll save these to standardize `validation` and any data passed to `predict`
  # Then scale & center `training`
  sds <- purrr::map_dbl(training, stats::sd, na.rm = TRUE)
  means <- purrr::map_dbl(training, mean, na.rm = TRUE)
  training <- center_and_scale(training, sds, means)

  # Re-order `importance`'s rows
  # so they match the column order of `training` and `validation`
  importance_order <- purrr::map_dbl(
    names(training),
    ~ which(importance[["term"]] == .x)
  )
  importance <- importance[importance_order, ][["estimate"]]
  training <- sweep(training, 2, importance, "*")

  # Now apply all the above to the validation set, if provided
  if (!is.null(validation)) {
    # `validation` was re-ordered to match `training` back in the bridge function
    # so we can scale, center, and weight without needing to worry about
    # column order
    validation <- center_and_scale(validation, sds, means)
    validation <- sweep(validation, 2, importance, "*")
  }

  list(
    training = training,
    validation = validation,
    importance = importance,
    sds = sds,
    means = means
  )

}

calc_aoa <- function(di) {
  # Section 2.5 in Meyer and Pebesma
  as.vector(
    stats::quantile(di, 0.75, na.rm = TRUE) + (1.5 * stats::IQR(di, na.rm = TRUE))
  )
}

center_and_scale <- function(x, sds, means) {
  sweep(x, 2, means, "-") / sweep(x, 2, sds, "/")
}

# Calculate minimum distances from each validation point to the training data
#
# If `validation` is `NULL`, then find the smallest distances between each
# point in `training` and the rest of the training data
calculate_dk <- function(training, validation = NULL) {

  if (is.null(validation)) {
    distances <- proxyC::dist(as.matrix(training))
    diag(distances) <- NA
  } else {
    distances <- proxyC::dist(as.matrix(validation), as.matrix(training))
  }

  apply(distances, 1, min, na.rm = TRUE)

}

check_di_validation <- function(training, validation) {

  # If NULL, nothing to validate or re-order, so just return NULL
  if (is.null(validation)) return(NULL)

  # Make sure that the validation set has the same columns, in the same order,
  # as the original training data
  validation <- validation$predictors

  if (
    !all(names(training)   %in% names(validation)) ||
    !all(names(validation) %in% names(training))
  ) {
    rlang::abort(
      "`training` and `validation` must contain all the same columns"
    )
  }
  # Re-order validation so that its columns are guaranteed to be in the
  # same order as those in `training`
  validation[names(training)]

}

check_di_importance <- function(training, importance) {

  importance <- tidy_importance(importance)

  # Make sure that all training variables have importance values
  #
  # Because we've already called check_di_validation, this also means all
  # predictors in `validation` have importance values

  all_importance <- all(names(training) %in% importance[["term"]])

  if (!all_importance) {
    rlang::abort(
      "All predictors must have an importance value in `importance`",
      call = rlang::caller_env()
    )
  }

  all_variables <- all(importance[["term"]] %in% names(training))
  if (!all_variables) {
    rlang::abort(
      "All variables with an importance value in `importance` must be included as predictors"
    )
  }

  importance
}

check_di_columns_numeric <- function(training, validation) {
  col_is_numeric <- c(
    purrr::map_lgl(training, is.numeric),
    purrr::map_lgl(validation, is.numeric)
  )

  if (!all(col_is_numeric)) {
    rlang::abort(
      "All predictors must be numeric",
      call = rlang::caller_env()
    )
  }
}

# -----------------------------------------------------------------------------
# ---------------------- Model function implementation ------------------------
# -----------------------------------------------------------------------------

predict_ww_area_of_applicability_numeric <- function(model, predictors) {

  if (!("ww_area_of_applicability" %in% class(model))) {
    rlang::abort(
      "`model` must be an `ww_area_of_applicability` object",
      call = rlang::caller_env()
    )
  }

  predictors <- center_and_scale(predictors, model$sds, model$means)
  predictors <- sweep(predictors, 2, model$importance, "*")
  dk <- calculate_dk(model$training, predictors)
  di <- dk / model$d_bar
  aoa <- di <= model$aoa_threshold

  tibble::tibble(
    di = di,
    aoa = aoa
  )
}

# -----------------------------------------------------------------------------
# ------------------------ Model function bridge ------------------------------
# -----------------------------------------------------------------------------

predict_ww_area_of_applicability_bridge <- function(type, model, predictors) {
  if (!all(names(model$training) %in% names(predictors))) {
    rlang::abort(
      "Some variables used to calculate the DI are missing from `new_data`",
      call = rlang::caller_env()
    )
  }
  predictors <- predictors[names(model$training)]
  predictors <- as.matrix(predictors)

  predict_function <- get_aoa_predict_function(type)
  predictions <- predict_function(model, predictors)

  hardhat::validate_prediction_size(predictions, predictors)

  predictions
}

# -----------------------------------------------------------------------------
# ----------------------- Model function interface ----------------------------
# -----------------------------------------------------------------------------

#' Predict from a `ww_area_of_applicability`
#'
#' @param object A `ww_area_of_applicability` object.
#'
#' @param new_data A data frame or matrix of new samples.
#'
#' @param type A single character. The type of predictions to generate.
#' Valid options are:
#'
#' - `"numeric"` for numeric predictions.
#'
#' @param ... Not used, but required for extensibility.
#'
#' @details The function computes the distance indices of the new data and
#' whether or not they are "inside" the area of applicability.
#'
#' @return
#'
#' A tibble of predictions. The number of rows in the tibble is guaranteed
#' to be the same as the number of rows in `new_data`.
#'
#' @family area of applicability functions
#'
#' @examplesIf rlang::is_installed("vip")
#' library(vip)
#' train <- gen_friedman(1000, seed = 101)  # ?vip::gen_friedman
#' test <- train[701:1000, ]
#' train <- train[1:700, ]
#' pp <- stats::ppr(y ~ ., data = train, nterms = 11)
#' importance <- vi_permute(
#'   pp,
#'   target = "y",
#'   metric = "rsquared",
#'   pred_wrapper = predict
#' )
#'
#' aoa <- ww_area_of_applicability(y ~ ., train, test, importance = importance)
#' predict(aoa, test)
#'
#' @export
predict.ww_area_of_applicability <- function(object, new_data, type = "numeric", ...) {
  forged <- hardhat::forge(new_data, object$blueprint)
  rlang::arg_match(type, valid_predict_types())
  predict_ww_area_of_applicability_bridge(type, object, forged$predictors)
}

delayedAssign("score", {
  if (rlang::is_installed("applicable")) {
    applicable::score
  } else {
    function(object, ...) {
      UseMethod("score")
    }
  }
})

score.ww_area_of_applicability <- predict.ww_area_of_applicability

# -----------------------------------------------------------------------------
# ----------------------- Helper functions ------------------------------------
# -----------------------------------------------------------------------------

get_aoa_predict_function <- function(type) {
  switch(
    type,
    numeric = predict_ww_area_of_applicability_numeric
  )
}

valid_predict_types <- function() {
  c("numeric")
}

#' Print number of predictors and area-of-applicability threshold
#'
#' @param x A `ww_area_of_applicability` object.
#'
#' @param digits The number of digits to print, used when rounding the AOA threshold.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @keywords internal
#'
#' @examplesIf rlang::is_installed("vip")
#' library(vip)
#' trn <- gen_friedman(500, seed = 101)  # ?vip::gen_friedman
#' pp <- ppr(y ~ ., data = trn, nterms = 11)
#' importance <- vi_permute(
#'   pp,
#'   target = "y",
#'   metric = "rsquared",
#'   pred_wrapper = predict
#' )
#'
#'
#' ww_area_of_applicability(trn[2:11], importance = importance)
#'
#' @export
print.ww_area_of_applicability <- function(x, digits = getOption("digits"), ...) {
  predictors_count <- ncol(x$blueprint$ptypes$predictors)
  aoa_threshold <- round(x$aoa_threshold, digits)

  print_output <- glue::glue(
    "# Predictors:
      {predictors_count}
   Area-of-applicability threshold:
      {aoa_threshold}"
  )

  cat(print_output)

  invisible(x)
}
