# Getis-Ord statistics are stable

    Code
      (df_local_g <- ww_local_getis_ord_g(guerry_modeled, crime_pers, predictions,
        ctg, wts))
    Output
      # A tibble: 85 x 4
         .metric           .estimator .estimate                               geometry
         <chr>             <chr>          <dbl>                         <MULTIPOLYGON>
       1 local_getis_ord_g standard      -0.353 (((381847 1762775, 381116 1763059, 37~
       2 local_getis_ord_g standard      -1.11  (((381847 1762775, 381116 1763059, 37~
       3 local_getis_ord_g standard      -0.929 (((381847 1762775, 381116 1763059, 37~
       4 local_getis_ord_g standard      -0.794 (((381847 1762775, 381116 1763059, 37~
       5 local_getis_ord_g standard      -2.22  (((381847 1762775, 381116 1763059, 37~
       6 local_getis_ord_g standard      -0.403 (((381847 1762775, 381116 1763059, 37~
       7 local_getis_ord_g standard      -0.152 (((381847 1762775, 381116 1763059, 37~
       8 local_getis_ord_g standard      -0.455 (((381847 1762775, 381116 1763059, 37~
       9 local_getis_ord_g standard      16.4   (((381847 1762775, 381116 1763059, 37~
      10 local_getis_ord_g standard      -1.97  (((381847 1762775, 381116 1763059, 37~
      # ... with 75 more rows

---

    Code
      (df_local_g_p <- ww_local_getis_ord_g_pvalue(guerry_modeled, crime_pers,
        predictions, ctg, wts))
    Output
      # A tibble: 85 x 4
         .metric                  .estimator .estimate                        geometry
         <chr>                    <chr>          <dbl>                  <MULTIPOLYGON>
       1 local_getis_ord_g_pvalue standard     0.339   (((381847 1762775, 381116 1763~
       2 local_getis_ord_g_pvalue standard     0.0175  (((381847 1762775, 381116 1763~
       3 local_getis_ord_g_pvalue standard     0.0470  (((381847 1762775, 381116 1763~
       4 local_getis_ord_g_pvalue standard     0.136   (((381847 1762775, 381116 1763~
       5 local_getis_ord_g_pvalue standard     0.253   (((381847 1762775, 381116 1763~
       6 local_getis_ord_g_pvalue standard     0.0943  (((381847 1762775, 381116 1763~
       7 local_getis_ord_g_pvalue standard     0.541   (((381847 1762775, 381116 1763~
       8 local_getis_ord_g_pvalue standard     0.132   (((381847 1762775, 381116 1763~
       9 local_getis_ord_g_pvalue standard     0.361   (((381847 1762775, 381116 1763~
      10 local_getis_ord_g_pvalue standard     0.00787 (((381847 1762775, 381116 1763~
      # ... with 75 more rows

---

    Code
      (df_local_g_both <- ww_local_g(guerry_modeled, crime_pers, predictions, ctg,
        wts))
    Output
      # A tibble: 170 x 4
         .metric           .estimator .estimate                               geometry
         <chr>             <chr>          <dbl>                         <MULTIPOLYGON>
       1 local_getis_ord_g standard      -0.353 (((381847 1762775, 381116 1763059, 37~
       2 local_getis_ord_g standard      -1.11  (((381847 1762775, 381116 1763059, 37~
       3 local_getis_ord_g standard      -0.929 (((381847 1762775, 381116 1763059, 37~
       4 local_getis_ord_g standard      -0.794 (((381847 1762775, 381116 1763059, 37~
       5 local_getis_ord_g standard      -2.22  (((381847 1762775, 381116 1763059, 37~
       6 local_getis_ord_g standard      -0.403 (((381847 1762775, 381116 1763059, 37~
       7 local_getis_ord_g standard      -0.152 (((381847 1762775, 381116 1763059, 37~
       8 local_getis_ord_g standard      -0.455 (((381847 1762775, 381116 1763059, 37~
       9 local_getis_ord_g standard      16.4   (((381847 1762775, 381116 1763059, 37~
      10 local_getis_ord_g standard      -1.97  (((381847 1762775, 381116 1763059, 37~
      # ... with 160 more rows

---

    Code
      (df_local_gstar <- ww_local_getis_ord_g_star(guerry_modeled, crime_pers,
        predictions, ctg, wts))
    Message <cliMessage>
      ! attr `self.include` is `TRUE`. Reporting Gi*.
    Output
      # A tibble: 85 x 4
         .metric                .estimator .estimate                          geometry
         <chr>                  <chr>          <dbl>                    <MULTIPOLYGON>
       1 local_getis_ord_g_star standard    -1.68e14 (((381847 1762775, 381116 176305~
       2 local_getis_ord_g_star standard    -2.74e14 (((381847 1762775, 381116 176305~
       3 local_getis_ord_g_star standard    -2.42e14 (((381847 1762775, 381116 176305~
       4 local_getis_ord_g_star standard     2.28e14 (((381847 1762775, 381116 176305~
       5 local_getis_ord_g_star standard     1.67e14 (((381847 1762775, 381116 176305~
       6 local_getis_ord_g_star standard     1.99e14 (((381847 1762775, 381116 176305~
       7 local_getis_ord_g_star standard    -2.21e14 (((381847 1762775, 381116 176305~
       8 local_getis_ord_g_star standard     3.24e14 (((381847 1762775, 381116 176305~
       9 local_getis_ord_g_star standard    -9.93e13 (((381847 1762775, 381116 176305~
      10 local_getis_ord_g_star standard     3.09e14 (((381847 1762775, 381116 176305~
      # ... with 75 more rows

---

    Code
      (df_local_gstar_p <- ww_local_getis_ord_g_star_pvalue(guerry_modeled,
        crime_pers, predictions, ctg, wts))
    Message <cliMessage>
      ! attr `self.include` is `TRUE`. Reporting Gi*.
    Output
      # A tibble: 85 x 4
         .metric                       .estimator .estimate                   geometry
         <chr>                         <chr>          <dbl>             <MULTIPOLYGON>
       1 local_getis_ord_g_star_pvalue standard           0 (((381847 1762775, 381116~
       2 local_getis_ord_g_star_pvalue standard           0 (((381847 1762775, 381116~
       3 local_getis_ord_g_star_pvalue standard           0 (((381847 1762775, 381116~
       4 local_getis_ord_g_star_pvalue standard           0 (((381847 1762775, 381116~
       5 local_getis_ord_g_star_pvalue standard           0 (((381847 1762775, 381116~
       6 local_getis_ord_g_star_pvalue standard           0 (((381847 1762775, 381116~
       7 local_getis_ord_g_star_pvalue standard           0 (((381847 1762775, 381116~
       8 local_getis_ord_g_star_pvalue standard           0 (((381847 1762775, 381116~
       9 local_getis_ord_g_star_pvalue standard           0 (((381847 1762775, 381116~
      10 local_getis_ord_g_star_pvalue standard           0 (((381847 1762775, 381116~
      # ... with 75 more rows

---

    Code
      (df_local_gstar_both <- ww_local_g_star(guerry_modeled, crime_pers, predictions,
        ctg, wts))
    Message <cliMessage>
      ! attr `self.include` is `TRUE`. Reporting Gi*.
      ! attr `self.include` is `TRUE`. Reporting Gi*.
    Output
      # A tibble: 170 x 4
         .metric                .estimator .estimate                          geometry
         <chr>                  <chr>          <dbl>                    <MULTIPOLYGON>
       1 local_getis_ord_g_star standard    -1.68e14 (((381847 1762775, 381116 176305~
       2 local_getis_ord_g_star standard    -2.74e14 (((381847 1762775, 381116 176305~
       3 local_getis_ord_g_star standard    -2.42e14 (((381847 1762775, 381116 176305~
       4 local_getis_ord_g_star standard     2.28e14 (((381847 1762775, 381116 176305~
       5 local_getis_ord_g_star standard     1.67e14 (((381847 1762775, 381116 176305~
       6 local_getis_ord_g_star standard     1.99e14 (((381847 1762775, 381116 176305~
       7 local_getis_ord_g_star standard    -2.21e14 (((381847 1762775, 381116 176305~
       8 local_getis_ord_g_star standard     3.24e14 (((381847 1762775, 381116 176305~
       9 local_getis_ord_g_star standard    -9.93e13 (((381847 1762775, 381116 176305~
      10 local_getis_ord_g_star standard     3.09e14 (((381847 1762775, 381116 176305~
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
       [1] -168308387803232 -274405572285170 -241858005162170  228390401715772
       [5]  166748014584862  198788401451248 -221320485362044  324173454017994
       [9]  -99284328357525  309074154948790  327184541521090  262696056087867
      [13] -120088916247991  292161339579883   35220244637808  -55856763564721
      [17] -232499951160756   53617437054654 -193326675525458 -229145515457402
      [21] -144338852512006   53136773108287  -13005755916436  222394324866485
      [25]  -11523226130481  -58550521384045 -278629685901850  345801852401069
      [29]  230367559473314  102281910155645  -38493424751105  292122470000938
      [33] -234484325306228 -124538153699584 -202459745414769   30264265612981
      [37] -201104438805887   -1650727697557 -154182501076507  -12624206288180
      [41]  158990389501258 -116050068584586   -1955328329450  189679778168526
      [45]   82346147313291  310810301906256 -114118312516990 -216346080329789
      [49] -175469259326990 -118685858918476 -348733705281085   33199108719391
      [53] -188256459211664 -177208457909145  106624681112522 -168351814884615
      [57] -295448506421821  -94142129938600 -201038189862294 -310584590503163
      [61]  -96460846089419  142232318205369  132602636845772  414046663455734
      [65]  168202739340077  271288340149131 -175297788186277 -124966071943760
      [69] -265712708256039 -235083897622010  140196754161316 -151028755075517
      [73]   19360948095172   45470692123277  -48884198627685 -241304715111519
      [77]  242413240964787  214206479077895  263954365708421  285300108401522
      [81]  -53316118634537  -86030469320124 -121426774866172   -1049952835098
      [85] -101545337048924

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

