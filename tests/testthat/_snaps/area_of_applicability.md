# `ww_area_of_applicability` is not defined for vectors

    `x` isn't a supported object type.
    i `ww_area_of_applicability()` can only handle data.frames, matrices, formulas, and recipes.
    x `x` is a numeric

# normal use

    Code
      predict(aoa, test)
    Output
      # A tibble: 300 x 2
             di aoa  
          <dbl> <lgl>
       1 0.175  TRUE 
       2 0.191  TRUE 
       3 0.190  TRUE 
       4 0.0802 TRUE 
       5 0.141  TRUE 
       6 0.287  TRUE 
       7 0.178  TRUE 
       8 0.205  TRUE 
       9 0.134  TRUE 
      10 0.177  TRUE 
      # ... with 290 more rows

---

    Code
      predict(aoa, train)
    Output
      # A tibble: 700 x 2
            di aoa  
         <dbl> <lgl>
       1     0 TRUE 
       2     0 TRUE 
       3     0 TRUE 
       4     0 TRUE 
       5     0 TRUE 
       6     0 TRUE 
       7     0 TRUE 
       8     0 TRUE 
       9     0 TRUE 
      10     0 TRUE 
      # ... with 690 more rows

# `new_ww_area_of_applicability` arguments are assigned correctly

    Code
      x$training
    Output
      # A tibble: 700 x 10
             x1    x2     x3    x4    x5    x6    x7     x8     x9    x10
          <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl>  <dbl>
       1 0.372  0.102 0.693  0.518 0.878 0.671 0.608 0.153  0.154  0.296 
       2 0.0438 0.602 0.776  0.509 0.118 0.375 0.108 0.0636 0.573  0.543 
       3 0.710  0.254 0.0180 0.715 0.334 0.640 0.423 0.423  0.233  0.181 
       4 0.658  0.542 0.230  0.177 0.474 0.508 0.241 0.0709 0.139  0.989 
       5 0.250  0.383 0.462  0.270 0.489 0.853 0.760 0.929  0.848  0.705 
       6 0.300  0.992 0.666  0.924 0.736 0.564 0.467 0.890  0.391  0.289 
       7 0.585  0.283 0.845  0.715 0.905 0.731 0.944 0.825  0.0876 0.0670
       8 0.333  0.858 0.697  0.260 0.517 0.370 0.830 0.823  0.839  0.860 
       9 0.622  0.490 0.468  0.572 0.682 0.384 0.451 0.769  0.979  0.975 
      10 0.546  0.476 0.829  0.192 0.456 0.944 0.574 0.623  0.358  0.0580
      # ... with 690 more rows

---

    Code
      x$importance
    Output
         term     estimate
      1    x1 0.4513503114
      2    x2 0.4050240509
      3    x3 0.1802543852
      4    x4 0.5782718534
      5    x5 0.1561788555
      6    x6 0.0009105000
      7    x7 0.0006527584
      8    x8 0.0012708119
      9    x9 0.0007807150
      10  x10 0.0011424776

---

    Code
      x$d_bar
    Output
      [1] 1.16272

---

    Code
      x$aoa_threshold
    Output
      [1] 0.3080491

---

    Code
      x$blueprint
    Output
      Formula blueprint: 
       
      # Predictors: 10 
        # Outcomes: 1 
         Intercept: FALSE 
      Novel Levels: FALSE 
       Composition: tibble 
        Indicators: traditional 

