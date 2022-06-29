#' Make 'nb' objects from sf objects
#'
#' @details
#' When `nb = NULL`, the method used to create neighbors from `data` is
#' dependent on what geometry type `data` is:
#'
#' + If `nb = NULL` and `data` is a point geometry
#' (classes "sfc_POINT" or "sfc_MULTIPOINT") the "nb" object will be created
#' using [ww_make_point_neighbors()].
#' + If `nb = NULL` and `data` is a polygon geometry
#' (classes "sfc_POLYGON" or "sfc_MULTIPOLYGON") the "nb" object will be created
#' using [ww_make_polygon_neighbors()].
#' + If `nb = NULL` and `data` is any other geometry type, the "nb" object will
#' be created using the centroids of the data as points, with a warning.
#'
#' @param ... Arguments passed to the neighbor-creating function.
#'
#'
#' @param data An sf object (of class "sf" or "sfc").
#' @param nb An object of class "nb" (in which case it will be returned
#' unchanged), or a function to create an object of class "nb" from `data` and
#' `...`, or `NULL`. See details.
#' @inheritParams rlang::abort
#'
#' @return An object of class "nb".
#'
#' @export
ww_build_neighbors <- function(data, nb = NULL, ..., call = rlang::caller_env()) {
  data <- sf::st_geometry(data)

  type <- if (any(c("sfc_MULTIPOINT", "sfc_POINT") %in% class(data))) {
    "point"
  } else if (any(c("sfc_MULTIPOLYGON", "sfc_POLYGON") %in% class(data))) {
    "polygon"
  } else if (is.null(nb)) {
    rlang::warn(
      c(
        "Non-point or polygon geometry specified, calculating neighbors using sf::st_centroid()",
        "i" = "To avoid this, provide neighbors explicitly",
        "i" = "Or provide a neighbor-creating function"
      ),
      call = call
    )
    data <- sf::st_centroid(data)
    "point"
  }

  if (is.null(nb)) {
    nb <- switch(
      type,
      "point" = ww_make_point_neighbors(data, ...),
      "polygon" = ww_make_polygon_neighbors(data, ...)
    )
  }

  if (rlang::is_function(nb)) {
    nb <- do.call(nb, list(data, ...))
  }

  if (!inherits(nb, "nb")) {
    rlang::abort(
      "Couldn't figure out how to build an `nb` object from the provided arguments",
      call = call
    )
  }

  nb

}

#' Make 'nb' objects from point geometries
#'
#' This function uses [spdep::knearneigh()] and [spdep::knn2nb()] to
#' create a "nb" neighbors list.
#'
#' @param data An `sfc_POINT` or `sfc_MULTIPOINT` object.
#' @param k How many nearest neighbors to use in [spdep::knearneigh()].
#' @param sym Force the output neighbors list (from [spdep::knn2nb()]) to
#' symmetry.
#' @param ... Other arguments passed to [spdep::knearneigh()].
#'
#' @return An object of class "nb"
#'
#' @export
ww_make_point_neighbors <- function(data, k = 1, sym = FALSE, ...) {

  knn <- spdep::knearneigh(data, k, ...)
  spdep::knn2nb(knn, sym = sym)

}

#' Make 'nb' objects from polygon geometries
#'
#' This function is an extremely thin wrapper around [spdep::poly2nb()],
#' renamed to use the waywiser "ww" prefix.
#'
#' @param data An `sfc_POLYGON` or `sfc_MULTIPOLYGON` object.
#' @param ... Additional arguments passed to [spdep::poly2nb()].
#'
#' @return An object of class "nb"
#'
#' @export
ww_make_polygon_neighbors <- function(data, ...) {
  spdep::poly2nb(data, ...)
}

#' Build "listw" objects of spatial weights
#'
#' @param x Either an sf object or a "nb" neighbors list object.
#' If an sf object, will be converted into a neighbors list via
#' [ww_build_neighbors()].
#' @param wt Either a "listw" object (which will be returned unchanged),
#' a function for creating a "listw" object from `x`, or `NULL`, in which case
#' weights will be constructed via [spdep::nb2listw()].
#' @param include_self Include each region itself in its own list of neighbors?
#' @param ... Arguments passed to the weight constructing function.
#'
#' @return A `listw` object.
#'
#' @export
ww_build_weights <- function(x, wt = NULL, include_self = FALSE, ...) {

  if (!inherits(x, "nb")) {
    x <- ww_build_neighbors(x, call = rlang::caller_env())
  }

  if (include_self) {
    if (!identical(attr(x, "self.included"), TRUE)) x <- spdep::include.self(x)
  }

  if (is.null(wt)) wt <- spdep::nb2listw(x, ...)

  if (rlang::is_function(wt)) {
    wt <- do.call(wt, list(x, ...))
  }

  if (!inherits(wt, "listw")) {
    rlang::abort(
      "Couldn't figure out how to build a `listw` object from the provided arguments",
      call = rlang::caller_env()
    )
  }

  wt

}
