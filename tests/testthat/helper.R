if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  Sys.setenv("waywiser_test_cast" = "true")
}

trip_dplyr_warning <- function() {
  invisible(
    suppressWarnings(
      dplyr::summarise(dplyr::group_by(iris, Species), 1:2)
    )
  )
}
