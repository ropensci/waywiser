# Global Geary statistics are stable

    Code
      df_global_c <- ww_global_geary_c(guerry_modeled, crime_pers, predictions, ctg,
        wts)
      df_global_c[1:3]
    Output
      # A tibble: 1 x 3
        .metric        .estimator .estimate
        <chr>          <chr>          <dbl>
      1 global_geary_c standard       0.565

---

    Code
      df_global_c_p <- ww_global_geary_pvalue(guerry_modeled, crime_pers, predictions,
        ctg, wts)
      df_global_c_p[1:3]
    Output
      # A tibble: 1 x 3
        .metric             .estimator .estimate
        <chr>               <chr>          <dbl>
      1 global_geary_pvalue standard     0.00200

---

    Code
      df_global_c_both <- ww_global_geary(guerry_modeled, crime_pers, predictions,
        ctg, wts)
      df_global_c_both[1:3]
    Output
      # A tibble: 2 x 3
        .metric             .estimator .estimate
        <chr>               <chr>          <dbl>
      1 global_geary_c      standard     0.565  
      2 global_geary_pvalue standard     0.00200

---

    Code
      (vec_global_c <- ww_global_geary_c_vec(guerry_modeled$crime_pers,
      guerry_modeled$predictions, ctg, wts))
    Output
      [1] 0.5654044

---

    Code
      (vec_global_c_p <- ww_global_geary_pvalue_vec(guerry_modeled$crime_pers,
      guerry_modeled$predictions, ctg, wts))
    Output
      [1] 0.002

