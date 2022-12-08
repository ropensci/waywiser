# ww_multi_scale

    Code
      made_w_grid_args
    Output
      # A tibble: 6 x 6
        .metric .estimator .estimate .grid_args       .grid          .notes          
        <chr>   <chr>          <dbl> <list>           <list>         <list>          
      1 rmse    standard      75108. <tibble [1 x 2]> <sf [344 x 3]> <tibble [0 x 2]>
      2 mae     standard      54833. <tibble [1 x 2]> <sf [344 x 3]> <tibble [0 x 2]>
      3 rmse    standard      82076. <tibble [1 x 2]> <sf [103 x 3]> <tibble [0 x 2]>
      4 mae     standard      60467. <tibble [1 x 2]> <sf [103 x 3]> <tibble [0 x 2]>
      5 rmse    standard      27862. <tibble [1 x 2]> <sf [5 x 3]>   <tibble [0 x 2]>
      6 mae     standard      23267. <tibble [1 x 2]> <sf [5 x 3]>   <tibble [0 x 2]>

---

    Some observations were not within any grid cell, and as such were not used in any assessments.
    i See the `.notes` column for details.

