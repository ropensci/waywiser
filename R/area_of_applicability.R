#' Find the area of applicability
#'
#' This function calculates the "area of applicability" of a model, as
#' introduced by Meyer and Pebesma (2021). While the initial paper introducing
#' this method focused on spatial models, there is nothing inherently spatial
#' about the method; it can be used with any type of data.
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
#' train <- vip::gen_friedman(1000, seed = 101)  # ?vip::gen_friedman
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
ww_area_of_applicability.data.frame <- function(x, testing = NULL, importance, ...) {
  rlang::check_dots_empty()
  training <- hardhat::mold(x, NA_real_)
  if (!is.null(testing)) testing <- hardhat::mold(testing, NA_real_)
  ww_area_of_applicability_impl(training, testing, importance, ...)
}

#' @export
#' @rdname ww_area_of_applicability
ww_area_of_applicability.matrix <- ww_area_of_applicability.data.frame

#' @export
#' @rdname ww_area_of_applicability
ww_area_of_applicability.formula <- function(x, data, testing = NULL, importance, ...) {
  rlang::check_dots_empty()
  training <- hardhat::mold(x, data)
  if (!is.null(testing)) testing <- hardhat::mold(x, testing)
  ww_area_of_applicability_impl(training, testing, importance, ...)
}

#' @export
#' @rdname ww_area_of_applicability
ww_area_of_applicability.recipe <- ww_area_of_applicability.formula

#' @export
#' @rdname ww_area_of_applicability
ww_area_of_applicability.rset <- function(x, y = NULL, importance, ...) {
  rlang::check_dots_empty()

  if (missing(y) || identical(y, NULL) || identical(y, NA)) y <- NA_real_

  # Standardize ALL data following CAST
  res <- standardize_and_weight(
    x$splits[[1]]$data, NULL, tidy_importance(importance)
  )
  browser()
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
      ww_area_of_applicability_impl(
        training,
        testing,
        importance,
        include_di = TRUE,
        means = res$means,
        sds = res$sds
      )
    }
  )

  aoa <- aoa_calcs[[1]]
  aoa$training <- res$training
  aoa$sds <- res$sds
  aoa$means <- res$means
  aoa$d_bar <- mean(unlist(purrr::map(aoa_calcs, purrr::chuck, "d_bar")))

  # di <- unlist(purrr::map(aoa_calcs, purrr::chuck, "di"))
  dk <- unlist(purrr::map(aoa_calcs, purrr::chuck, "dk"))
  di <- dk / aoa$d_bar
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

ww_area_of_applicability_prep <- function(training, testing, importance) {
  training <- training$predictors

  testing <- check_di_testing(training, testing)

  importance <- check_di_importance(training, importance)
  check_di_columns_numeric(training, testing)

  list(
    training = training,
    testing = testing,
    importance = importance
  )
}

ww_area_of_applicability_impl <- function(training, testing, importance, ..., include_di = FALSE,
                                          sds = NULL, means = NULL) {

  blueprint <- training$blueprint
  res <- ww_area_of_applicability_prep(training, testing, importance)

  # Comments reference section numbers from Meyer and Pebesma 2021
  # (doi: 10.1111/2041-210X.13650)

  # 2.1 Standardization of predictor variables
  # 2.2 Weighting of variables
  res$training <- center_and_scale(res$training, sds, means)
  res$testing <- center_and_scale(res$testing, sds, means)
  di <- calc_di(res$training, res$testing, res$importance)
  aoa_threshold <- calc_aoa(di$di)

  aoa <- hardhat::new_model(
    training = res$training,
    importance = res$importance,
    sds = sds,
    means = means,
    di = di$di,
    dk = di$dk,
    d_bar = di$d_bar,
    aoa_threshold = aoa_threshold,
    blueprint = blueprint,
    class = "ww_area_of_applicability"
  )
  if (!include_di) aoa["di"] <- NULL

  aoa
}

calc_di <- function(training, testing, importance) {

  # 2.3 Multivariate distance calculation
  # Calculates the distance between each point in the `testing` set
  # (or `training`, if `testing` is `NULL`)
  # to the closest point in the training set
  dk <- calculate_dk(training, testing)

  # 2.4 Dissimilarity index
  # Find the mean nearest neighbor distance between training points:
  dists <- proxyC::dist(as.matrix(training))
  diag(dists) <- NA
  d_bar <- Matrix::mean(dists, na.rm = TRUE)

  list(
    # Use d_bar to rescale dk from 2.3
    dk = dk,
    di = dk / d_bar,
    d_bar = d_bar
  )

}

standardize_and_weight <- function(training, testing, importance) {
  # Store standard deviations and means of all predictors from training
  # We'll save these to standardize `testing` and any data passed to `predict`
  # Then scale & center `training`
  sds <- purrr::map_dbl(training, stats::sd, na.rm = TRUE)
  means <- purrr::map_dbl(training, mean, na.rm = TRUE)
  training <- center_and_scale(training, sds, means)

  # Re-order `importance`'s rows
  # so they match the column order of `training` and `testing`
  importance_order <- purrr::map_dbl(
    names(training),
    ~ which(importance[["term"]] == .x)
  )
  importance <- importance[importance_order, ][["estimate"]]
  training <- sweep(training, 2, importance, "*")

  # Now apply all the above to the testing set, if provided
  if (!is.null(testing)) {
    # `testing` was re-ordered to match `training` back in the bridge function
    # so we can scale, center, and weight without needing to worry about
    # column order
    testing <- center_and_scale(testing, sds, means)
    testing <- sweep(testing, 2, importance, "*")
  }

  list(
    training = training,
    testing = testing,
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
  sweep(x, 2, means, "-") |> sweep(2, sds, "/")
}

# Calculate minimum distances from each testing point to the training data
#
# If `testing` is `NULL`, then find the smallest distances between each
# point in `training` and the rest of the training data
calculate_dk <- function(training, testing = NULL) {
  if (is.null(testing)) {
    distances <- proxyC::dist(as.matrix(training))
    diag(distances) <- NA
  } else {
    distances <- proxyC::dist(as.matrix(testing), as.matrix(training))
  }

  apply(distances, 1, min, na.rm = TRUE)

}

check_di_testing <- function(training, testing) {

  # If NULL, nothing to validate or re-order, so just return NULL
  if (is.null(testing)) return(NULL)

  # Make sure that the testing set has the same columns, in the same order,
  # as the original training data
  testing <- testing$predictors

  if (
    !all(names(training)   %in% names(testing)) ||
    !all(names(testing) %in% names(training))
  ) {
    rlang::abort(
      "`training` and `testing` must contain all the same columns"
    )
  }
  # Re-order testing so that its columns are guaranteed to be in the
  # same order as those in `training`
  testing[names(training)]

}

check_di_importance <- function(training, importance) {

  importance <- tidy_importance(importance)

  # Make sure that all training variables have importance values
  #
  # Because we've already called check_di_testing, this also means all
  # predictors in `testing` have importance values

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

check_di_columns_numeric <- function(training, testing) {
  col_is_numeric <- c(
    purrr::map_lgl(training, is.numeric),
    purrr::map_lgl(testing, is.numeric)
  )

  if (!all(col_is_numeric)) {
    rlang::abort(
      "All predictors must be numeric",
      call = rlang::caller_env()
    )
  }
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
#' @param ... Not used
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
predict.ww_area_of_applicability <- function(object, new_data, ...) {
  forged <- hardhat::forge(new_data, object$blueprint)

  if (!all(names(object$training) %in% names(forged$predictors))) {
    rlang::abort(
      "Some variables used to calculate the DI are missing from `new_data`",
      call = rlang::caller_env()
    )
  }

  predictors <- forged$predictors[names(object$training)]
  predictors <- as.matrix(predictors)

  predictors <- center_and_scale(predictors, object$sds, object$means)
  predictors <- sweep(predictors, 2, object$importance, "*")
  dk <- calculate_dk(object$training, predictors)
  di <- dk / object$d_bar
  aoa <- di <= object$aoa_threshold

  predictions <- tibble::tibble(
    di = di,
    aoa = aoa
  )

  hardhat::validate_prediction_size(predictions, predictors)

  predictions
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
