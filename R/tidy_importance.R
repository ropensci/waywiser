
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
