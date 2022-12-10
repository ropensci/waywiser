# waywiser (development version)

* Added functions (primarily `ww_multi_scale()`) and a vignette for multi-scale 
  assessment of model predictions.
  
* Rewrote README and moved the old content to a new vignette on assessing the
  spatial dependency in model residuals.
  
* Added functions to calculate metrics from Ji and Gallo (2006) and Willmott
  (1981): `ww_agreement_coefficient()`, `ww_systematic_agreement_coefficient()`, `ww_unsystematic_agreement_coefficient()`, `ww_unsystematic_mpd(),`, `ww_systematic_mpd()`, `ww_unsystematic_rmpd()`, `ww_systematic_rmpd()`, `ww_willmott_d()`, `ww_systematic_mse()`, `ww_unsystematic_mse()`, `ww_systematic_rmse()`, `ww_unsystematic_rmse()`.

* Added a dependency on FNN.

* Changed a call in `ww_area_of_applicability()` to use FNN for nearest 
  neighbors, rather than fields. This sped up prediction by a _lot_.

# waywiser 0.2.0

* Added functions for calculating the area of applicability of a model. 

# waywiser 0.1.0

* Added functions for automatically constructing `nb` and `listw` objects

* Added functions for global and local Geary's C values.

* Added functions for local Getis-Ord G and G* values.

* Added functions for global and local Moran's I values.

* Added a `NEWS.md` file to track changes to the package.
