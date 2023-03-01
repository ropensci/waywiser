# ww_multi_scale

    Code
      made_w_grid_args
    Output
      # A tibble: 6 x 6
        .metric .estimator .estimate .grid_args       .grid          .notes          
        <chr>   <chr>          <dbl> <list>           <list>         <list>          
      1 rmse    standard      75108. <tibble [1 x 2]> <sf [344 x 5]> <tibble [0 x 2]>
      2 mae     standard      54833. <tibble [1 x 2]> <sf [344 x 5]> <tibble [0 x 2]>
      3 rmse    standard      82076. <tibble [1 x 2]> <sf [103 x 5]> <tibble [0 x 2]>
      4 mae     standard      60467. <tibble [1 x 2]> <sf [103 x 5]> <tibble [0 x 2]>
      5 rmse    standard      27862. <tibble [1 x 2]> <sf [5 x 5]>   <tibble [0 x 2]>
      6 mae     standard      23267. <tibble [1 x 2]> <sf [5 x 5]>   <tibble [0 x 2]>

---

    Code
      ww_multi_scale(ames_sf, Sale_Price, predictions, grids = grids[1], metrics = yardstick::rmse)
    Output
      # A tibble: 1 x 6
        .metric .estimator .estimate .grid_args       .grid          .notes          
        <chr>   <chr>          <dbl> <list>           <list>         <list>          
      1 rmse    standard      75108. <tibble [0 x 0]> <sf [344 x 5]> <tibble [0 x 2]>

---

    Code
      ww_multi_scale(ames_sf, Sale_Price, predictions, n = list(c(1, 1)),
      autoexpand_grid = FALSE)
    Warning <rlang_warning>
      Some observations were not within any grid cell, and as such were not used in any assessments.
      i See the `.notes` column for details.
    Output
      # A tibble: 2 x 6
        .metric .estimator .estimate .grid_args       .grid        .notes          
        <chr>   <chr>          <dbl> <list>           <list>       <list>          
      1 rmse    standard        51.1 <tibble [1 x 1]> <sf [1 x 5]> <tibble [1 x 2]>
      2 mae     standard        51.1 <tibble [1 x 1]> <sf [1 x 5]> <tibble [1 x 2]>

# expected errors

    Code
      ww_multi_scale(guerry_modeled, Crm_prs, predictions, n = list(c(1, 1)),
      metrics = yardstick::rmse)
    Error <rlang_error>
      ww_multi_scale is currently only implemented for point geometries.
      i Consider casting your data to points.

---

    Code
      ww_multi_scale(suppressWarnings(sf::st_centroid(guerry_modeled)), Crm_prs,
      predictions, n = list(c(1, 1)), na_rm = c(TRUE, FALSE), metrics = yardstick::rmse)
    Error <rlang_error>
      Only one logical value can be passed to `na_rm`.

---

    Code
      ww_multi_scale(iris, Sepal.Length, Sepal.Width, n = list(c(1, 1)), metrics = yardstick::rmse)
    Error <simpleError>
      no applicable method for 'ww_multi_scale' applied to an object of class "data.frame"

# srr: expected failures for ww_multi_scale

    Code
      ww_multi_scale(worldclim_predicted, predicted, response, n = c(2, 4))
    Error <rlang_error>
      `truth` must be numeric.

---

    Code
      ww_multi_scale(worldclim_predicted, response, predicted, n = c(2, 4))
    Error <rlang_error>
      `estimate` must be numeric.

---

    Code
      ww_multi_scale(worldclim_predicted, predicted, response, n = c(2, 4))
    Error <rlang_error>
      `truth` must be numeric.

---

    Code
      ww_multi_scale(worldclim_predicted, response, predicted, n = c(2, 4))
    Error <rlang_error>
      `estimate` must be numeric.

---

    Code
      ww_multi_scale(worldclim_predicted, predicted, response, n = 2)
    Output
      # A tibble: 2 x 6
        .metric .estimator .estimate .grid_args       .grid        .notes          
        <chr>   <chr>          <dbl> <list>           <list>       <list>          
      1 rmse    standard     0.00739 <tibble [1 x 1]> <sf [4 x 5]> <tibble [0 x 2]>
      2 mae     standard     0.00691 <tibble [1 x 1]> <sf [4 x 5]> <tibble [0 x 2]>

---

    Code
      ww_multi_scale(worldclim_predicted, response, predicted, n = 2)
    Output
      # A tibble: 2 x 6
        .metric .estimator .estimate .grid_args       .grid        .notes          
        <chr>   <chr>          <dbl> <list>           <list>       <list>          
      1 rmse    standard     0.00739 <tibble [1 x 1]> <sf [4 x 5]> <tibble [0 x 2]>
      2 mae     standard     0.00691 <tibble [1 x 1]> <sf [4 x 5]> <tibble [0 x 2]>

---

    Code
      ww_multi_scale(worldclim_predicted, predicted, response, n = 2, na_rm = FALSE)
    Output
      # A tibble: 2 x 6
        .metric .estimator .estimate .grid_args       .grid        .notes          
        <chr>   <chr>          <dbl> <list>           <list>       <list>          
      1 rmse    standard          NA <tibble [1 x 1]> <sf [4 x 5]> <tibble [0 x 2]>
      2 mae     standard          NA <tibble [1 x 1]> <sf [4 x 5]> <tibble [0 x 2]>

---

    Code
      ww_multi_scale(worldclim_predicted, response, predicted, n = 2, na_rm = FALSE)
    Output
      # A tibble: 2 x 6
        .metric .estimator .estimate .grid_args       .grid        .notes          
        <chr>   <chr>          <dbl> <list>           <list>       <list>          
      1 rmse    standard          NA <tibble [1 x 1]> <sf [4 x 5]> <tibble [0 x 2]>
      2 mae     standard          NA <tibble [1 x 1]> <sf [4 x 5]> <tibble [0 x 2]>

---

    Code
      ww_multi_scale(head(worldclim_predicted, 0), response, predicted, n = c(2, 4))
    Error <rlang_error>
      0 rows were passed to `data`.

---

    Code
      ww_multi_scale(head(worldclim_predicted, 0), predicted, response, n = c(2, 4))
    Error <rlang_error>
      0 rows were passed to `data`.

---

    Code
      ww_multi_scale(worldclim_predicted, response, predicted, n = c(2, 4))
    Output
      # A tibble: 4 x 6
        .metric .estimator .estimate .grid_args       .grid         .notes          
        <chr>   <chr>          <dbl> <list>           <list>        <list>          
      1 rmse    standard         NaN <tibble [1 x 1]> <sf [4 x 5]>  <tibble [0 x 2]>
      2 mae     standard         NaN <tibble [1 x 1]> <sf [4 x 5]>  <tibble [0 x 2]>
      3 rmse    standard         NaN <tibble [1 x 1]> <sf [16 x 5]> <tibble [0 x 2]>
      4 mae     standard         NaN <tibble [1 x 1]> <sf [16 x 5]> <tibble [0 x 2]>

---

    Code
      ww_multi_scale(worldclim_predicted, predicted, response, n = c(2, 4))
    Output
      # A tibble: 4 x 6
        .metric .estimator .estimate .grid_args       .grid         .notes          
        <chr>   <chr>          <dbl> <list>           <list>        <list>          
      1 rmse    standard         NaN <tibble [1 x 1]> <sf [4 x 5]>  <tibble [0 x 2]>
      2 mae     standard         NaN <tibble [1 x 1]> <sf [4 x 5]>  <tibble [0 x 2]>
      3 rmse    standard         NaN <tibble [1 x 1]> <sf [16 x 5]> <tibble [0 x 2]>
      4 mae     standard         NaN <tibble [1 x 1]> <sf [16 x 5]> <tibble [0 x 2]>

---

    Code
      ww_multi_scale(worldclim_simulation, response, response, n = c(2, 4))
    Output
      # A tibble: 4 x 6
        .metric .estimator .estimate .grid_args       .grid         .notes          
        <chr>   <chr>          <dbl> <list>           <list>        <list>          
      1 rmse    standard           0 <tibble [1 x 1]> <sf [4 x 5]>  <tibble [0 x 2]>
      2 mae     standard           0 <tibble [1 x 1]> <sf [4 x 5]>  <tibble [0 x 2]>
      3 rmse    standard           0 <tibble [1 x 1]> <sf [16 x 5]> <tibble [0 x 2]>
      4 mae     standard           0 <tibble [1 x 1]> <sf [16 x 5]> <tibble [0 x 2]>

# raster method errors as expected

    Code
      ww_multi_scale(truth = 1)
    Error <simpleError>
      no applicable method for 'ww_multi_scale' applied to an object of class "c('double', 'numeric')"

---

    Code
      ww_multi_scale(truth = c(terra::rast(r1), terra::rast(r1)))
    Error <rlang_error>
      `truth` must be a SpatRaster with only one layer.

---

    Code
      ww_multi_scale(truth = terra::rast(r1), estimate = 1)
    Error <rlang_error>
      `estimate` must be a SpatRaster with only one layer.

---

    Code
      ww_multi_scale(truth = terra::rast(r1), estimate = c(terra::rast(r1), terra::rast(
        r1)))
    Error <rlang_error>
      `estimate` must be a SpatRaster with only one layer.

