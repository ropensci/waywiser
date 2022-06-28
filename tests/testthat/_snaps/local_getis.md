# Getis-Ord statistics are stable

    Code
      df_local_g <- ww_local_getis_ord_g(guerry_modeled, crime_pers, predictions, ctg,
        wts)
      df_local_g[1:3]
    Output
      # A tibble: 85 x 3
         .metric           .estimator .estimate
         <chr>             <chr>          <dbl>
       1 local_getis_ord_g standard      -0.353
       2 local_getis_ord_g standard      -1.11 
       3 local_getis_ord_g standard      -0.929
       4 local_getis_ord_g standard      -0.794
       5 local_getis_ord_g standard      -2.22 
       6 local_getis_ord_g standard      -0.403
       7 local_getis_ord_g standard      -0.152
       8 local_getis_ord_g standard      -0.455
       9 local_getis_ord_g standard      16.4  
      10 local_getis_ord_g standard      -1.97 
      # ... with 75 more rows

---

    Code
      df_local_g_p <- ww_local_getis_ord_g_pvalue(guerry_modeled, crime_pers,
        predictions, ctg, wts)
      df_local_g_p[1:3]
    Output
      # A tibble: 85 x 3
         .metric                  .estimator .estimate
         <chr>                    <chr>          <dbl>
       1 local_getis_ord_g_pvalue standard     0.339  
       2 local_getis_ord_g_pvalue standard     0.0175 
       3 local_getis_ord_g_pvalue standard     0.0470 
       4 local_getis_ord_g_pvalue standard     0.136  
       5 local_getis_ord_g_pvalue standard     0.253  
       6 local_getis_ord_g_pvalue standard     0.0943 
       7 local_getis_ord_g_pvalue standard     0.541  
       8 local_getis_ord_g_pvalue standard     0.132  
       9 local_getis_ord_g_pvalue standard     0.361  
      10 local_getis_ord_g_pvalue standard     0.00787
      # ... with 75 more rows

---

    Code
      df_local_g_both <- ww_local_g(guerry_modeled, crime_pers, predictions, ctg, wts)
      df_local_g_both[1:3]
    Output
      # A tibble: 170 x 3
         .metric           .estimator .estimate
         <chr>             <chr>          <dbl>
       1 local_getis_ord_g standard      -0.353
       2 local_getis_ord_g standard      -1.11 
       3 local_getis_ord_g standard      -0.929
       4 local_getis_ord_g standard      -0.794
       5 local_getis_ord_g standard      -2.22 
       6 local_getis_ord_g standard      -0.403
       7 local_getis_ord_g standard      -0.152
       8 local_getis_ord_g standard      -0.455
       9 local_getis_ord_g standard      16.4  
      10 local_getis_ord_g standard      -1.97 
      # ... with 160 more rows

---

    Code
      df_local_gstar <- ww_local_getis_ord_g_star(guerry_modeled, crime_pers,
        predictions, ctg, wts)
    Message <cliMessage>
      ! attr `self.include` is `TRUE`. Reporting Gi*.
    Code
      df_local_gstar[1:3]
    Output
      # A tibble: 85 x 3
         .metric                .estimator .estimate
         <chr>                  <chr>          <dbl>
       1 local_getis_ord_g_star standard    -1.68e14
       2 local_getis_ord_g_star standard    -2.74e14
       3 local_getis_ord_g_star standard    -2.42e14
       4 local_getis_ord_g_star standard     2.28e14
       5 local_getis_ord_g_star standard     1.67e14
       6 local_getis_ord_g_star standard     1.99e14
       7 local_getis_ord_g_star standard    -2.21e14
       8 local_getis_ord_g_star standard     3.24e14
       9 local_getis_ord_g_star standard    -9.93e13
      10 local_getis_ord_g_star standard     3.09e14
      # ... with 75 more rows

---

    Code
      df_local_gstar_p <- ww_local_getis_ord_g_star_pvalue(guerry_modeled, crime_pers,
        predictions, ctg, wts)
    Message <cliMessage>
      ! attr `self.include` is `TRUE`. Reporting Gi*.
    Code
      df_local_gstar_p[1:3]
    Output
      # A tibble: 85 x 3
         .metric                       .estimator .estimate
         <chr>                         <chr>          <dbl>
       1 local_getis_ord_g_star_pvalue standard           0
       2 local_getis_ord_g_star_pvalue standard           0
       3 local_getis_ord_g_star_pvalue standard           0
       4 local_getis_ord_g_star_pvalue standard           0
       5 local_getis_ord_g_star_pvalue standard           0
       6 local_getis_ord_g_star_pvalue standard           0
       7 local_getis_ord_g_star_pvalue standard           0
       8 local_getis_ord_g_star_pvalue standard           0
       9 local_getis_ord_g_star_pvalue standard           0
      10 local_getis_ord_g_star_pvalue standard           0
      # ... with 75 more rows

---

    Code
      df_local_gstar_both <- ww_local_g_star(guerry_modeled, crime_pers, predictions,
        ctg, wts)
    Message <cliMessage>
      ! attr `self.include` is `TRUE`. Reporting Gi*.
      ! attr `self.include` is `TRUE`. Reporting Gi*.
    Code
      df_local_gstar_both[1:3]
    Output
      # A tibble: 170 x 3
         .metric                .estimator .estimate
         <chr>                  <chr>          <dbl>
       1 local_getis_ord_g_star standard    -1.68e14
       2 local_getis_ord_g_star standard    -2.74e14
       3 local_getis_ord_g_star standard    -2.42e14
       4 local_getis_ord_g_star standard     2.28e14
       5 local_getis_ord_g_star standard     1.67e14
       6 local_getis_ord_g_star standard     1.99e14
       7 local_getis_ord_g_star standard    -2.21e14
       8 local_getis_ord_g_star standard     3.24e14
       9 local_getis_ord_g_star standard    -9.93e13
      10 local_getis_ord_g_star standard     3.09e14
      # ... with 160 more rows

---

    Code
      (vec_local_g <- ww_local_getis_ord_g_vec(guerry_modeled$crime_pers,
      guerry_modeled$predictions, ctg, wts))
    Output
       [1] -0.35262151 -1.11368651 -0.92866064 -0.79356385 -2.22275711 -0.40336719
       [7] -0.15203590 -0.45454838 16.38902247 -1.96710667 -0.66444373 -1.02600524
      [13]  2.12734140 -4.25914217  0.41630063  1.56536706 -3.78671247 -0.15623283
      [19] -0.30816054 -0.58785407 -0.08687604  1.21572052  0.56126961 -0.83912766
      [25]  0.23427920 -0.96629229 -0.59696130 -1.33567941 -4.75988338 -2.12022122
      [31] -0.04579763 -1.06038525 -3.22352867 -0.13533331  6.81843300 -0.54495320
      [37] -0.73789320  0.27216991 -3.56911954  0.09349007 -1.03076037  4.99995636
      [43]  0.16855776 -0.22995006  0.99945545 -0.56871201 -0.21585772 -0.36762836
      [49]  1.42199807 -0.37136266 -1.10024910  0.40178905 -0.98686978 -1.54768209
      [55] -0.03215295 -0.84160434 -1.13552334 -0.16840506 -0.54281373 -3.17850146
      [61]  1.16213681 -1.18963391 -0.25767637 -0.68888730 -0.46826171 -0.33959174
      [67]  5.24414049 -1.29782722 -0.78124761 -0.33678092 -0.43286265  3.59421668
      [73]  0.35919751 -0.01742732  1.16696681 -0.33931265 -0.84230044 -1.03567754
      [79] -0.95523145 -1.15704654 -1.95461580  0.67049808  1.08446225  0.20026305
      [85]  1.84442238

---

    Code
      (vec_local_g_p <- ww_local_getis_ord_g_pvalue_vec(guerry_modeled$crime_pers,
      guerry_modeled$predictions, ctg, wts))
    Output
       [1] 0.338551650 0.017476897 0.046989319 0.136306243 0.252759855 0.094280941
       [7] 0.540950240 0.131507964 0.360573261 0.007868825 0.002910297 0.099812583
      [13] 0.263145960 0.006073623 0.486840848 0.592079221 0.024897187 0.769018856
      [19] 0.147064254 0.236079422 0.566712648 0.491109230 0.430473715 0.079265433
      [25] 0.725081238 0.578327557 0.252363140 0.001922916 0.033158491 0.345911465
      [31] 0.980042792 0.026524059 0.020365959 0.457589915 0.049929480 0.842223381
      [37] 0.122974384 0.896634027 0.134780339 0.852933638 0.197999212 0.283637329
      [43] 0.908861719 0.251729216 0.309816435 0.029894833 0.388820107 0.244965500
      [49] 0.055290126 0.358699720 0.004749006 0.419371223 0.135285432 0.172833170
      [55] 0.889256614 0.166182979 0.052600987 0.625967137 0.123280301 0.042396581
      [61] 0.263286551 0.358596865 0.672797323 0.064221214 0.333500223 0.360499055
      [67] 0.106951191 0.292784000 0.020687637 0.123175709 0.582501078 0.195554540
      [73] 0.718377209 0.944328466 0.556059177 0.167383256 0.060941608 0.081390165
      [79] 0.130453836 0.012380565 0.707571437 0.277582701 0.190935930 0.922672415
      [85] 0.296789212

---

    Code
      (vec_local_gstar <- ww_local_getis_ord_g_star_vec(guerry_modeled$crime_pers,
      guerry_modeled$predictions, ctg, wts))
    Message <cliMessage>
      ! attr `self.include` is `TRUE`. Reporting Gi*.
    Output
       [1] -1.683084e+14 -2.744056e+14 -2.418580e+14  2.283904e+14  1.667480e+14
       [6]  1.987884e+14 -2.213205e+14  3.241735e+14 -9.928433e+13  3.090742e+14
      [11]  3.271845e+14  2.626961e+14 -1.200889e+14  2.921613e+14  3.522024e+13
      [16] -5.585676e+13 -2.325000e+14  5.361744e+13 -1.933267e+14 -2.291455e+14
      [21] -1.443389e+14  5.313677e+13 -1.300576e+13  2.223943e+14 -1.152323e+13
      [26] -5.855052e+13 -2.786297e+14  3.458019e+14  2.303676e+14  1.022819e+14
      [31] -3.849342e+13  2.921225e+14 -2.344843e+14 -1.245382e+14 -2.024597e+14
      [36]  3.026427e+13 -2.011044e+14 -1.650728e+12 -1.541825e+14 -1.262421e+13
      [41]  1.589904e+14 -1.160501e+14 -1.955328e+12  1.896798e+14  8.234615e+13
      [46]  3.108103e+14 -1.141183e+14 -2.163461e+14 -1.754693e+14 -1.186859e+14
      [51] -3.487337e+14  3.319911e+13 -1.882565e+14 -1.772085e+14  1.066247e+14
      [56] -1.683518e+14 -2.954485e+14 -9.414213e+13 -2.010382e+14 -3.105846e+14
      [61] -9.646085e+13  1.422323e+14  1.326026e+14  4.140467e+14  1.682027e+14
      [66]  2.712883e+14 -1.752978e+14 -1.249661e+14 -2.657127e+14 -2.350839e+14
      [71]  1.401968e+14 -1.510288e+14  1.936095e+13  4.547069e+13 -4.888420e+13
      [76] -2.413047e+14  2.424132e+14  2.142065e+14  2.639544e+14  2.853001e+14
      [81] -5.331612e+13 -8.603047e+13 -1.214268e+14 -1.049953e+12 -1.015453e+14

---

    Code
      (vec_local_gstar_p <- ww_local_getis_ord_g_star_pvalue_vec(guerry_modeled$
        crime_pers, guerry_modeled$predictions, ctg, wts))
    Message <cliMessage>
      ! attr `self.include` is `TRUE`. Reporting Gi*.
    Output
       [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      [39] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      [77] 0 0 0 0 0 0 0 0 0

