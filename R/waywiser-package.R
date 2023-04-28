## usethis namespace: start
#' @keywords internal
"_PACKAGE"

#' @importFrom rlang enquo .data .env
#' @importFrom stats predict complete.cases na.fail
#' @importFrom yardstick new_numeric_metric yardstick_any_missing
#' @importFrom yardstick yardstick_remove_missing
utils::globalVariables(c(".truth", ".estimate", ".grid_idx", "grid_cell_idx"))
## usethis namespace: end
NULL
