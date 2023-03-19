# waywiser 0.3.0

## Breaking Changes

* Removed combination functions -- `ww_global_geary`, `ww_global_moran`, 
  `ww_local_geary`, `ww_local_moran`, `ww_local_getis_ord`. Use `metric_set()`
  to combine functions instead.

* Renamed `ww_local_getis_ord_pvalue_vec()` and variants to 
  `ww_local_getis_ord_g_pvalue_vec()`; this change allows internal functions to
  work properly, and makes it easier for the output to indicate if the p-value
  is associated with a g or g* value.

* Yardstick metrics will no longer include geometry columns in their returns.

* The `na_action` argument to `ww_area_of_applicability()` has been replaced by
  `na_rm`, with a default value of `FALSE`. 
  
* `na_rm` is now `TRUE` by default for non-spatial-autocorrelation functions.
  NA values will cause spatial-autocorrelation functions to fail with an error.

## New Features

* Added functions (primarily `ww_multi_scale()`) and a vignette for multi-scale 
  assessment of model predictions.

* Added functions to calculate metrics from Ji and Gallo (2006) and Willmott
  (1981, 1982, 2012): `ww_agreement_coefficient()`, 
  `ww_systematic_agreement_coefficient()`, 
  `ww_unsystematic_agreement_coefficient()`, `ww_unsystematic_mpd(),`,
  `ww_systematic_mpd()`, `ww_unsystematic_rmpd()`, `ww_systematic_rmpd()`,
  `ww_willmott_d()`, `ww_willmott_dr()`, `ww_willmott_d1()`, 
  `ww_systematic_mse()`, `ww_unsystematic_mse()`, `ww_systematic_rmse()`,
  `ww_unsystematic_rmse()`. Note that `ww_willmott_dr()` uses the version
  from Willmott (2012); other implementations (sometimes called "d1r") seem to 
  use an unbounded variant that I haven't found a reference to support.

* Changed a call in `ww_area_of_applicability()` to use FNN for nearest 
  neighbors, rather than fields. This sped up prediction by a _lot_.

## Bug Fixes

* Rewrote README and moved the old content to a new vignette on assessing the
  spatial dependency in model residuals.
  
* Added a dependency on FNN.

* Minimum version for dplyr has been bumped to 1.1.0

# waywiser 0.2.0

* Added functions for calculating the area of applicability of a model. 

# waywiser 0.1.0

* Added functions for automatically constructing `nb` and `listw` objects

* Added functions for global and local Geary's C values.

* Added functions for local Getis-Ord G and G* values.

* Added functions for global and local Moran's I values.

* Added a `NEWS.md` file to track changes to the package.
