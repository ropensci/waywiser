#' Find the area of applicability
#'
#' This function calculates the "area of applicability" of a model, as
#' introduced by Meyer and Pebesma (2021). While the initial paper introducing
#' this method focused on spatial models, there is nothing inherently spatial
#' about the method; it can be used with any type of data (and, because it does
#' not care about the spatial arrangement of your data, can be used with 2D or
#' 3D spatial data, and with geographic or projected CRS).
#'
#' Predictions made on points "inside" the area of applicability should be as
#' accurate as predictions made on the data provided to `testing`.
#' That means that generally `testing` should be your final hold-out
#' set so that predictions on points inside the area of applicability are
#' accurately described by your reported model metrics.
#' When passing an `rset` object to `x`, predictions made on points "inside" the
#' area of applicability instead should be as accurate as predictions made on
#' the assessment sets during cross-validation.
#'
#' @section Differences from CAST:
#' This implementation differs from
#' Meyer and Pebesma (2021) (and therefore from CAST) when using cross-validated
#' data in order to minimize data leakage. Namely, in order to calculate
#' the dissimilarity index \eqn{DI_{k}}, CAST:
#'
#' 1. Rescales all data used for cross validation at once, lumping assessment
#'    folds in with analysis data.
#' 2. Calculates a single \eqn{\bar{d}} as the mean distance between all points
#'    in the rescaled data set, including between points in the same assessment
#'    fold.
#' 3. For each point \eqn{k} that's used in an assessment fold, calculates
#'    \eqn{d_{k}} as the minimum distance between \eqn{k} and any point in its
#'    corresponding analysis fold.
#' 4. Calculates \eqn{DI_{k}} by dividing \eqn{d_{k}} by \eqn{\bar{d}} (which
#'    was partially calculated as the distance between \eqn{k} and the rest of
#'    the rescaled data).
#'
#' Because assessment data is used to calculate constants for rescaling analysis
#' data and \eqn{\bar{d}}, the assessment data may appear too "similar" to
#' the analysis data when calculating \eqn{DI_{k}}. As such, waywiser treats
#' each fold in an `rset` independently:
#'
#' 1. Each analysis set is rescaled independently.
#' 2. Separate \eqn{\bar{d}} are calculated for each fold, as the mean distance
#'    between all points in the analysis set for that fold.
#' 3. Identically to CAST, \eqn{d_{k}} is the minimum distance between a point
#'    \eqn{k} in the assessment fold and any point in the
#'    corresponding analysis fold.
#' 4. \eqn{DI_{k}} is then found by dividing \eqn{d_{k}} by \eqn{\bar{d}},
#'    which was calculated independently from {k}.
#'
#' Predictions are made using the full training data set, rescaled once (in
#' the same way as CAST), and the mean \eqn{\bar{d}} across folds, under the
#' assumption that the "final" model in use will be retrained using the entire
#' data set.
#'
#' In practice, this means waywiser produces very slightly higher \eqn{\bar{d}}
#' values than CAST and a slightly higher area of applicability threshold than
#' CAST when using `rset` objects.
#'
#' @param x Either a data frame, matrix, formula
#' (specifying predictor terms on the right-hand side), recipe
#' (from [recipes::recipe()], or `rset` object, produced by resampling functions
#' from rsample or spatialsample.
#'
#' If `x` is a recipe, it should be the same one used to pre-process the data
#' used in your model. If the recipe used to build the area of applicability
#' doesn't match the one used to build the model, the returned area of
#' applicability won't be correct.
#'
#' @param y Optional: a recipe (from [recipes::recipe()]) or formula.
#'
#' If `y` is a recipe, it should be the same one used to pre-process the data
#' used in your model. If the recipe used to build the area of applicability
#' doesn't match the one used to build the model, the returned area of
#' applicability won't be correct.
#'
#' @param data The data frame representing your "training" data, when using the
#'  formula or recipe methods.
#'
#' @param testing A data frame or matrix containing the data used to
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
#'     variable in the training and testing data, and `estimate`, containing
#'     the (raw or scaled) feature importance for each variable.
#'   * An object of class `vi` with at least two columns, `Variable` and
#'     `Importance`.
#'
#' All variables in the training data (`x` or `data`, depending on the context)
#' must have a matching importance estimate, and all terms with importance
#' estimates must be in the training data.
#'
#' @param na_rm A logical of length 1, indicating whether observations (in both
#' training and testing) with `NA` values in predictors should be removed. Only
#' predictor variables are considered, and this value has no impact on
#' predictions (where `NA` values produce `NA` predictions). If `na_rm = FALSE`
#' and `NA` values are found, this function returns an error.
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
#' A `ww_area_of_applicability` object, which can be used with [predict()] to
#' calculate the distance of new data to the original training data, and
#' determine if new data is within a model's area of applicability.
#'
#' @family area of applicability functions
#'
#' @examplesIf rlang::is_installed("vip")
#' train <- vip::gen_friedman(1000, seed = 101) # ?vip::gen_friedman
#' test <- train[701:1000, ]
#' train <- train[1:700, ]
#' pp <- stats::ppr(y ~ ., data = train, nterms = 11)
#' importance <- vip::vi_permute(
#'   pp,
#'   target = "y",
#'   metric = "rsquared",
#'   pred_wrapper = predict
#' )
#'
#' aoa <- ww_area_of_applicability(y ~ ., train, test, importance = importance)
#' predict(aoa, test)
#'
#' # Equivalent methods for calculating AOA:
#' ww_area_of_applicability(train[2:11], test[2:11], importance)
#' ww_area_of_applicability(
#'   as.matrix(train[2:11]),
#'   as.matrix(test[2:11]),
#'   importance
#' )
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

#' @exportS3Method
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

#' @exportS3Method
#' @rdname ww_area_of_applicability
ww_area_of_applicability.data.frame <- function(x, testing = NULL, importance, ..., na_rm = FALSE) {
  rlang::check_dots_empty()
  training <- hardhat::mold(x, NA_real_)
  if (!is.null(testing)) testing <- hardhat::mold(testing, NA_real_)
  create_aoa(training, testing, importance, ..., na_rm = na_rm)
}

#' @exportS3Method
#' @rdname ww_area_of_applicability
ww_area_of_applicability.matrix <- ww_area_of_applicability.data.frame

#' @exportS3Method
#' @rdname ww_area_of_applicability
ww_area_of_applicability.formula <- function(x, data, testing = NULL, importance, ..., na_rm = FALSE) {
  rlang::check_dots_empty()

  # This method is also used for recipes, which don't need blueprints:
  blueprint <- NULL
  if (inherits(x, "formula")) {
    blueprint <- hardhat::default_formula_blueprint(indicators = "none")
  }

  training <- hardhat::mold(x, data, blueprint = blueprint)
  processed_testing <- NULL
  if (!is.null(testing)) {
    processed_testing <- hardhat::mold(x, testing, blueprint = blueprint)
  }


  # Catch non-numeric, non-base classes
  # cf https://github.com/tidymodels/hardhat/issues/219
  if (any(
    purrr::map_lgl(
      as.data.frame(data)[names(data) %in% names(training$predictors)],
      function(x) !(inherits(x, "numeric") || inherits(x, "integer"))
    ),
    purrr::map_lgl(
      as.data.frame(testing)[names(testing) %in% names(processed_testing$predictors)],
      function(x) !(inherits(x, "numeric") || inherits(x, "integer"))
    )
  )) {
    rlang::abort(
      "All variables in `data` and `testing` must inherit either numeric or integer classes."
    )
  }

  create_aoa(training, processed_testing, importance, ..., na_rm = na_rm)
}

#' @exportS3Method
#' @rdname ww_area_of_applicability
ww_area_of_applicability.recipe <- ww_area_of_applicability.formula

#' @exportS3Method
#' @rdname ww_area_of_applicability
ww_area_of_applicability.rset <- function(x, y = NULL, importance, ..., na_rm = FALSE) {
  rlang::check_dots_empty()
  rlang::check_installed("rsample")

  if (missing(y) || identical(y, NULL) || identical(y, NA)) y <- NA_real_

  aoa_calcs <- purrr::map(
    x$splits,
    function(rsplit) {
      training <- rsample::analysis(rsplit)
      testing <- rsample::assessment(rsplit)

      if (identical(y, NA_real_)) {
        training <- hardhat::mold(training, y)
        testing <- hardhat::mold(testing, y)
      } else {
        training <- hardhat::mold(y, training)
        testing <- hardhat::mold(y, testing)
      }
      create_aoa(
        training,
        testing,
        importance,
        include_di = TRUE,
        na_rm = na_rm
      )
    }
  )

  aoa <- aoa_calcs[[1]]
  training <- if (identical(y, NA_real_)) {
    hardhat::mold(x$splits[[1]]$data, NA_real_)$predictors
  } else {
    hardhat::mold(y, x$splits[[1]]$data)$predictors
  }
  if (na_rm) {
    training <- training[complete_cases(training), , drop = FALSE]
  }
  aoa$sds <- purrr::map_dbl(training, stats::sd)
  aoa$means <- purrr::map_dbl(training, mean)
  aoa$transformed_training <- standardize_and_weight(
    training,
    aoa$sds,
    aoa$means,
    aoa$importance
  )

  aoa$d_bar <- mean(unlist(purrr::map(aoa_calcs, purrr::chuck, "d_bar")))

  di <- unlist(purrr::map(aoa_calcs, purrr::chuck, "di"))
  aoa["di"] <- NULL
  aoa$aoa_threshold <- calc_aoa(di)

  aoa
}

# Comments reference section numbers from Meyer and Pebesma 2021
# (doi: 10.1111/2041-210X.13650)

#' Workhorse function to create AOA objects from all input types
#'
#' @inheritParams ww_area_of_applicability
#' @param include_di Boolean: include DI in the returned object? Necessary for
#' post-processing AOA objects when working with rsets.
#'
#' @return
#' A `ww_area_of_applicability` object, which can be used with [predict()] to
#' calculate the distance of new data to the original training data, and
#' determine if new data is within a model's area of applicability.
#'
#' @noRd
create_aoa <- function(training, testing, importance, na_rm, ..., include_di = FALSE) {
  aoa <- list(
    training = training$predictors,
    class = "ww_area_of_applicability",
    blueprint = training$blueprint
  )

  if (length(na_rm) != 1) {
    rlang::abort("Only one value can be passed to `na_rm`.")
  }

  if (na_rm) {
    aoa$training <- aoa$training[complete_cases(aoa$training), , drop = FALSE]
  } else if (yardstick_any_missing(aoa$training, NULL, NULL)) {
    rlang::abort(
      c(
        "Missing values in training data.",
        i = "Either process your data to fix NA values, or set `na_rm = TRUE`."
      )
    )
  }

  aoa$testing <- check_di_testing(aoa$training, testing, na_rm)

  if (nrow(aoa$training) == 0) {
    rlang::abort(
      "0 rows were passed as training data."
    )
  }

  if (!is.null(testing) && nrow(aoa$testing) == 0) {
    rlang::abort(
      "0 rows were passed as testing data."
    )
  }

  check_di_columns_numeric(aoa$training, aoa$testing)

  aoa$importance <- check_di_importance(aoa$training, importance)

  # 2.1 Standardization of predictor variables
  # Store standard deviations and means of all predictors from training
  # We'll save these to standardize `testing`
  # Then scale & center `training`
  aoa$sds <- purrr::map_dbl(aoa$training, stats::sd)
  aoa$means <- purrr::map_dbl(aoa$training, mean)

  aoa$transformed_training <- standardize_and_weight(
    aoa$training,
    aoa$sds,
    aoa$means,
    aoa$importance
  )

  if (!is.null(aoa$testing)) {
    # We can freely overwrite testing here;
    # we don't need the untransformed version
    aoa$testing <- standardize_and_weight(
      aoa$testing,
      aoa$sds,
      aoa$means,
      aoa$importance
    )
  }

  aoa$d_bar <- calc_d_bar(aoa$transformed_training)
  aoa$di <- calc_di(aoa$transformed_training, aoa$testing, aoa$d_bar)
  aoa$aoa_threshold <- calc_aoa(aoa$di)

  if (isTRUE(all.equal(aoa$aoa_threshold, 0))) {
    rlang::warn(
      "The AOA threshold was 0, which is usually unexpected.",
      i = "Did you accidentally pass the same data as testing and training?"
    )
  }

  aoa <- aoa[c(
    "transformed_training",
    "sds",
    "means",
    "importance",
    "di",
    "d_bar",
    "aoa_threshold",
    "blueprint",
    "class"
  )]
  if (!include_di) aoa["di"] <- NULL

  do.call(hardhat::new_model, aoa)
}

#' Validate "testing" objects and reorder columns to match training data
#'
#' @param training The data frame representing your "training" data.
#'
#' @param testing The output from [hardhat::mold()],
#' containing the data used to validate your model.
#' This should be the same data as used to calculate all model accuracy metrics.
#'
#' @return `testing`, with columns re-ordered to match `training`
#'
#' @noRd
check_di_testing <- function(training, testing, na_rm = FALSE) {
  # If NULL, nothing to validate or re-order, so just return NULL
  if (is.null(testing)) {
    return(NULL)
  }

  testing <- testing$predictors

  if (!is.na(na_rm) && na_rm) {
    testing <- testing[complete_cases(testing), , drop = FALSE]
  } else if (!is.na(na_rm) && yardstick_any_missing(testing, NULL, NULL)) {
    rlang::abort(
      c(
        "Missing values in testing data.",
        i = "Either process your data to fix NA values, or set `na_rm = TRUE`."
      )
    )
  }

  # Make sure that the testing set has the same columns, in the same order,
  # as the original training data
  if (!all(names(training) %in% names(testing))) {
    rlang::abort(
      "Some columns in `training` were not present in `testing` (or `new_data`)."
    )
  }
  # Re-order testing so that its columns are guaranteed to be in the
  # same order as those in `training`
  testing[names(training)]
}

#' Validate "importance" objects
#'
#' @param training The data frame representing your "training" data.
#'
#' @param importance Any object accepted by [tidy_importance()].
#'
#' @return A standardized importance data frame, with columns "term" and
#' "importance", with terms ordered in the same order as columns in training
#' data.
#'
#' @noRd
check_di_importance <- function(training, importance) {
  importance <- tidy_importance(importance)

  # Make sure that all training variables have importance values
  #
  # Because we've already called check_di_testing, this also means all
  # predictors in `testing` have importance values
  all_importance <- all(names(training) %in% importance[["term"]])
  if (!all_importance) {
    rlang::abort(
      "All predictors must have an importance value in `importance`.",
      call = rlang::caller_env(2)
    )
  }

  all_variables <- all(importance[["term"]] %in% names(training))
  if (!all_variables) {
    rlang::abort(
      "All variables with an importance value in `importance` must be included as predictors.",
      call = rlang::caller_env(2)
    )
  }

  # Re-order `importance`'s rows
  # so they match the column order of `training` and `testing`
  importance_order <- purrr::map_dbl(
    names(training),
    ~ which(importance[["term"]] == .x)
  )
  importance[importance_order, ]
}

#' Validate all predictor columns are numeric
#'
#' @param training The data frame representing your "training" data.
#'
#' @param testing The output from [check_di_testing()].
#'
#' @return TRUE, invisibly.
#'
#' @noRd
check_di_columns_numeric <- function(training, testing) {
  col_is_numeric <- c(
    purrr::map_lgl(training, is.numeric),
    purrr::map_lgl(testing, is.numeric)
  )

  if (!all(col_is_numeric)) {
    rlang::abort(
      "All predictors must be numeric.",
      call = rlang::caller_env(4)
    )
  }

  return(invisible(TRUE))
}

#' Center and scale variables, and weight by variable importance
#'
#' @param dat A data frame with all numeric columns
#' @param sds The standard deviation of each variable in `dat`, in the same
#' order as `dat`
#' @param means The mean of each variable in `dat`, in the same order as `dat`
#' @param importance The output of [check_di_importance()].
#'
#' @return A data.frame in the same shape as `dat`, with standardized and
#' weighted variables.
#'
#' @noRd
standardize_and_weight <- function(dat, sds, means, importance) {
  # 2.1 Standardize
  dat <- sweep(dat, 2, means, "-")
  dat <- sweep(dat, 2, sds, "/")
  # 2.2 Weighting of variables
  sweep(dat, 2, importance[["estimate"]], "*")
}

#' Calculate d_bar: The mean distance between training points.
#'
#' @param training The data frame representing your "training" data, after being
#' run through [standardize_and_weight()].
#'
#' @return A numeric of length 1.
#'
#' @noRd
calc_d_bar <- function(training) {
  # 2.4 Dissimilarity index
  # Find the mean nearest neighbor distance between training points:
  dists <- fields::rdist(training)
  diag(dists) <- NA
  Matrix::mean(dists, na.rm = TRUE)
}

#' Calculate di
#'
#' di is the nearest neighbor distance of each point in "testing" to the
#' training data (or, in the absence of testing data, of each point in training
#' to the rest of the training set), divided by d_bar.
#'
#' @param training The data frame representing your "training" data, after being
#' run through [standardize_and_weight()].
#'
#' @param testing The data frame representing your "testing" data, after being
#' run through [standardize_and_weight()].
#'
#' @param d_bar The output of [calc_d_bar()].
#'
#' @return A numeric of length `nrow(training)`.
#'
#' @noRd
calc_di <- function(training, testing, d_bar) {
  # 2.3 Multivariate distance calculation
  # Calculates the distance between each point in the `testing` set
  # (or `training`, if `testing` is `NULL`)
  # to the closest point in the training set
  if (is.null(testing)) {
    dk <- FNN::knn.dist(training, 1)[, 1]
  } else {
    dk <- FNN::knnx.dist(training, testing, 1)[, 1]
  }

  # Use d_bar to rescale dk from 2.3
  dk / d_bar
}

#' Calculate the area of applicability threshold
#'
#' @param di The output of [calc_di()].
#'
#' @return A numeric of length 1.
#'
#' @noRd
calc_aoa <- function(di) {
  # Section 2.5 in Meyer and Pebesma
  as.vector(
    stats::quantile(di, 0.75) +
      (1.5 * stats::IQR(di))
  )
}

#' Predict from a `ww_area_of_applicability`
#'
#' @param object A `ww_area_of_applicability` object.
#'
#' @param new_data A data frame or matrix of new samples.
#'
#' @param ... Not used.
#'
#' @details The function computes the distance indices of the new data and
#' whether or not they are "inside" the area of applicability.
#'
#' @return
#'
#' A tibble of predictions, with two columns: `di`, numeric, contains the
#' "dissimilarity index" of each point in `new_data`, while `aoa`, logical,
#' contains whether a row is inside (`TRUE`) or outside (`FALSE`) the area of
#' applicability.
#'
#' Note that this function is often called using
#' [terra::predict()], in which case `aoa` will be converted to numeric
#' implicitly; `1` values correspond to cells "inside" the area of applicability
#' and `0` corresponds to cells "outside" the AOA.
#'
#' The number of rows in the tibble is guaranteed
#' to be the same as the number of rows in `new_data`. Rows with `NA` predictor
#' values will have `NA` `di` and `aoa` values.
#'
#' @family area of applicability functions
#'
#' @examplesIf rlang::is_installed("vip")
#' library(vip)
#' train <- gen_friedman(1000, seed = 101) # ?vip::gen_friedman
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
#' @exportS3Method
predict.ww_area_of_applicability <- function(object, new_data, ...) {
  new_data <- hardhat::forge(new_data, object$blueprint)

  new_data <- check_di_testing(object$transformed_training, new_data, NA)
  existing_new_data <- complete.cases(new_data)

  check_di_columns_numeric(object$transformed_training, new_data)

  new_data <- standardize_and_weight(
    new_data,
    object$sds,
    object$means,
    object$importance
  )

  predictions <- tibble::tibble(
    di = NA_real_,
    aoa = NA,
    .rows = nrow(new_data)
  )

  predictions[existing_new_data, ]$di <- calc_di(
    object$transformed_training,
    new_data[existing_new_data, ],
    object$d_bar
  )
  predictions$aoa <- predictions$di <= object$aoa_threshold

  hardhat::validate_prediction_size(predictions, new_data)

  predictions
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
#' trn <- gen_friedman(500, seed = 101) # ?vip::gen_friedman
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
