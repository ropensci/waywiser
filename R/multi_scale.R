#' Evaluate metrics at multiple scales of aggregation
#'
#' @details
#' The grid blocks can be controlled by passing arguments to
#' [sf::st_make_grid()] via `...`. Some particularly useful arguments include:
#'
#' * `cellsize`: Target cellsize, expressed as the "diameter" (shortest
#' straight-line distance between opposing sides; two times the apothem)
#' of each block, in map units.
#' * `n`: The number of grid blocks in the x and y direction (columns, rows).
#' * `square`: A logical value indicating whether to create square (`TRUE`) or
#' hexagonal (`FALSE`) cells.
#'
#' If both `cellsize` and `n` are provided, then the number of blocks requested
#' by `n` of sizes specified by `cellsize` will be returned, likely not
#' lining up with the bounding box of `data`. If only `cellsize`
#' is provided, this function will return as many blocks of size
#' `cellsize` as fit inside the bounding box of `data`. If only `n` is provided,
#' then `cellsize` will be automatically adjusted to create the requested
#' number of cells.
#'
#' @param data A point geometry `sf` object containing the columns specified by
#' the truth and estimate arguments.
#' @inheritParams yardstick::rmse
#' @param metrics Either a [yardstick::metric_set()] object, or a list of
#' functions which will be used to construct a [yardstick::metric_set()] object
#' specifying the performance metrics to evaluate at each scale.
#' @param grids Optionally, a list of pre-computed `sf` or `sfc` objects
#' specifying polygon boundaries to
#' @inheritParams sf::st_make_grid
#' @param aggregation_function The function to use to aggregate predictions and
#' true values at various scales, by default [mean()]. You can pass any function
#' which takes a single vector and returns a scalar.
#'
#' @return A tibble with five columns: `.metric`, with the name
#' of the metric that the row describes; `.estimator`, with the name of the
#' estimator used, `.estimate`, with the output of the metric function;
#' `.grid_args`, with the arguments passed to [sf::st_make_grid()] via `...`
#' (if any), and `.grid`, containing the grids used to aggregate predictions.
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(ames, package = "modeldata")
#' ames_sf <- sf::st_as_sf(ames, coords = c("Longitude", "Latitude"), crs = 4326)
#' ames_model <- lm(Sale_Price ~ Lot_Area, data = ames_sf)
#' ames_sf$predictions <- predict(ames_model, ames_sf)
#'
#' ww_multi_scale(
#'   ames_sf,
#'   Sale_Price,
#'   predictions,
#'   n = list(
#'     c(20, 20),
#'     c(10, 10),
#'     c(1, 1)
#'   ),
#'   square = FALSE
#' )
#'
#' # or, equivalently:
#' grids <- list(
#'   sf::st_make_grid(ames_sf, n = c(20, 20), square = FALSE),
#'   sf::st_make_grid(ames_sf, n = c(10, 10), square = FALSE),
#'   sf::st_make_grid(ames_sf, n = c(1, 1), square = FALSE)
#' )
#' ww_multi_scale(ames_sf, Sale_Price, predictions, grids = grids)
#'
#' @export
ww_multi_scale <- function(
    data,
    truth,
    estimate,
    metrics = list(yardstick::rmse, yardstick::mae),
    grids = NULL,
    ...,
    aggregation_function = mean
) {
  .truth <- .estimate <- NULL

  geom_type <- unique(sf::st_geometry_type(data))
  if (!(length(geom_type) == 1 && geom_type == "POINT")) {
    rlang::abort(
      c(
        "ww_multi_scale is currently only implemented for point geometries.",
        i = "Consider casting your data to points."
      )
    )
  }

  if (!inherits(metrics, "metric_set")) {
    metrics <- do.call(yardstick::metric_set, metrics)
  }

  if (missing(grids)) {
    grid_args <- rlang::list2(...)
    grid_args <- tibble::as_tibble(do.call(cbind, grid_args))
    grids <- apply(
      grid_args,
      1,
      function(g_args) {
        do.call(
          sf::st_make_grid,
          c(g_args, x = list(data))
        )
      }
    )
  }

  grid_intersections <- purrr::map(
    grids,
    function(grid) {
      out <- sf::st_intersects(grid, data)
      out[purrr::map_lgl(out, function(x) !identical(x, integer(0)))]
    }
  )

  grid_intersections <- purrr::map(
    grid_intersections,
    function(idx_list) {
      purrr::map_dfr(
        idx_list,
        function(idx) dplyr::summarise(
          data[idx, , drop = FALSE],
          .truth = rlang::exec(aggregation_function, {{ truth }}),
          .estimate = rlang::exec(mean, {{ estimate }})
        )
      )
    }
  )

  if (!exists("grid_args")) {
    grid_args <- tibble::tibble()
    grid_arg_idx <- 0
  } else {
    grid_arg_idx <- seq_len(nrow(grid_args))
  }

  purrr::pmap_dfr(
    list(
      dat = grid_intersections,
      grid = grids,
      grid_arg = grid_arg_idx
    ),
    function(dat, grid, grid_arg) {
      out <- metrics(dat, .truth, .estimate)
      out[attr(out, "sf_column")] <- NULL
      out$.grid_args <- list(grid_args[grid_arg, ])
      out$.grid <- list(sf::st_as_sf(grid))
      out
    }
  )
}
