# This file generates helpers for compatibility with srr
# and is not a part of the waywiser package proper

devtools::load_all()
read_utf8 <- function(x) base::readLines(x, encoding = "UTF-8", warn = FALSE)

nonspatial_yardstick <- c(
  "ww_agreement_coefficient",
  "ww_systematic_agreement_coefficient",
  "ww_unsystematic_agreement_coefficient",
  "ww_unsystematic_mpd",
  "ww_systematic_mpd",
  "ww_unsystematic_rmpd",
  "ww_systematic_rmpd",
  "ww_willmott_d",
  "ww_willmott_d1",
  "ww_willmott_dr",
  "ww_systematic_mse",
  "ww_unsystematic_mse",
  "ww_systematic_rmse",
  "ww_unsystematic_rmse"
)

nonspatial_yardstick_template <- read_utf8("inst/srr_template_nonspatial_yardstick.R")

for (name in nonspatial_yardstick) {
  n_sims <- ""
  tolerance <- ""
  generated_template <- whisker::whisker.render(nonspatial_yardstick_template)
  generated_template <- c(
    "# This file was generated, do not edit by hand",
    "# Please edit inst/srr_template_nonspatial_yardstick.R instead",
    "",
    generated_template
  )
  writeLines(
    generated_template,
    file.path("tests", "testthat", paste0("test-srr-", name, ".R"))
  )
}

spatial_yardstick <- c(
  "ww_global_moran_i",
  "ww_global_moran_pvalue",
  "ww_local_moran_i",
  "ww_local_moran_pvalue",
  "ww_global_geary_c",
  "ww_global_geary_pvalue",
  "ww_local_geary_c",
  "ww_local_geary_pvalue",
  "ww_local_getis_ord_g",
  "ww_local_getis_ord_g_pvalue"
)

spatial_yardstick_template <- read_utf8("inst/srr_template_spatial_yardstick.R")

for (name in spatial_yardstick) {
  n_sims <- switch(
    name,
    "ww_local_geary_pvalue" = ", nsim = 10000",
    "ww_local_getis_ord_g_pvalue" = ", nsim = 10000",
    ""
  )
  tolerance <- switch(
    name,
    "ww_local_geary_pvalue" = ", tolerance = 0.1",
    "ww_local_getis_ord_g_pvalue" = ", tolerance = 0.03",
    ""
  )

  generated_template <- whisker::whisker.render(spatial_yardstick_template)
  generated_template <- c(
    "# This file was generated, do not edit by hand",
    "# Please edit inst/srr_template_spatial_yardstick.R instead",
    "",
    generated_template
  )
  writeLines(
    generated_template,
    file.path("tests", "testthat", paste0("test-srr-", name, ".R"))
  )
}
