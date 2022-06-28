# Local Geary statistics are stable

    Code
      df_local_c <- ww_local_geary_c(guerry_modeled, crime_pers, predictions, ctg,
        wts)
      df_local_c[1:3]
    Output
      # A tibble: 85 x 3
         .metric       .estimator .estimate
         <chr>         <chr>          <dbl>
       1 local_geary_c standard       0.981
       2 local_geary_c standard       0.836
       3 local_geary_c standard       0.707
       4 local_geary_c standard       0.108
       5 local_geary_c standard       0.264
       6 local_geary_c standard       1.36 
       7 local_geary_c standard       3.64 
       8 local_geary_c standard       1.57 
       9 local_geary_c standard       0.867
      10 local_geary_c standard       0.737
      # ... with 75 more rows

---

    Code
      df_local_c_p <- ww_local_geary_pvalue(guerry_modeled, crime_pers, predictions,
        ctg, wts)
      df_local_c_p[1:3]
    Output
      # A tibble: 85 x 3
         .metric            .estimator .estimate
         <chr>              <chr>          <dbl>
       1 local_geary_pvalue standard       0.885
       2 local_geary_pvalue standard       0.865
       3 local_geary_pvalue standard       0.928
       4 local_geary_pvalue standard       0.937
       5 local_geary_pvalue standard       0.832
       6 local_geary_pvalue standard       0.933
       7 local_geary_pvalue standard       0.801
       8 local_geary_pvalue standard       0.890
       9 local_geary_pvalue standard       0.602
      10 local_geary_pvalue standard       0.779
      # ... with 75 more rows

---

    Code
      df_local_c_both <- ww_local_geary(guerry_modeled, crime_pers, predictions, ctg,
        wts)
      df_local_c_both[1:3]
    Output
      # A tibble: 170 x 3
         .metric       .estimator .estimate
         <chr>         <chr>          <dbl>
       1 local_geary_c standard       0.981
       2 local_geary_c standard       0.836
       3 local_geary_c standard       0.707
       4 local_geary_c standard       0.108
       5 local_geary_c standard       0.264
       6 local_geary_c standard       1.36 
       7 local_geary_c standard       3.64 
       8 local_geary_c standard       1.57 
       9 local_geary_c standard       0.867
      10 local_geary_c standard       0.737
      # ... with 160 more rows

---

    Code
      (vec_local_c <- ww_local_geary_c_vec(guerry_modeled$crime_pers, guerry_modeled$
        predictions, ctg, wts))
    Output
       [1] 0.981119438 0.836402177 0.707464373 0.108332465 0.264075824 1.361485477
       [7] 3.641239412 1.571824022 0.867252524 0.737094462 0.573376555 0.001605731
      [13] 1.891988440 1.152840284 1.029320931 0.297642850 1.219953394 1.934113868
      [19] 1.632566652 0.441916658 5.202733790 0.921953310 3.084515822 0.237218594
      [25] 1.346684045 1.051652204 0.419414691 0.217280214 0.794409207 0.243971372
      [31] 0.376678958 0.139152907 0.711305633 3.096840680 1.974463944 0.922230710
      [37] 1.032031759 0.339464386 0.933794842 1.910440700 0.937597672 0.625628647
      [43] 0.376707677 2.692250283 1.288784962 0.798443065 1.671895951 1.310183326
      [49] 2.347513577 0.845204889 0.302940809 2.291804447 0.881999216 0.412051312
      [55] 2.006031605 0.561239582 0.375776092 1.853716391 1.191472387 1.146802970
      [61] 1.857618679 0.149044974 0.614228825 0.755373475 1.287962784 1.447534518
      [67] 1.236607966 0.962394651 0.338400653 1.914478855 0.641340157 2.146993342
      [73] 0.703881855 1.417638272 0.692636715 1.765618175 0.246058853 0.700262130
      [79] 0.002876896 0.057575267 0.420878038 2.025012395 2.525093274 1.053335832
      [85] 1.030009749

---

    Code
      (vec_local_c_p <- ww_local_geary_pvalue_vec(guerry_modeled$crime_pers,
      guerry_modeled$predictions, ctg, wts))
    Output
       [1] 0.88544191 0.86525549 0.92765287 0.93719614 0.83187131 0.93256580
       [7] 0.80143632 0.88979840 0.60227107 0.77922654 0.98623933 0.91978704
      [13] 0.18044847 0.42243657 0.70511930 0.90051807 0.37765871 0.28145872
      [19] 0.97306847 0.92745453 0.71720612 0.61271234 0.29110017 0.93326315
      [25] 0.57159635 0.49533890 0.88634524 0.95823272 0.68637825 0.92061864
      [31] 0.91275148 0.92729126 0.79442798 0.72012206 0.03844014 0.60176320
      [37] 0.80652079 0.85719274 0.59498542 0.58193244 0.67683169 0.72871659
      [43] 0.90993935 0.88513298 0.39362700 0.96963818 0.86027821 0.90720345
      [49] 0.07762623 0.90044360 0.97030404 0.35723240 0.78461325 0.87345459
      [55] 0.52298564 0.91365792 0.91901054 0.68542189 0.88564968 0.52850804
      [61] 0.15186839 0.86576012 0.82882315 0.90333044 0.71075775 0.82999351
      [67] 0.38141031 0.63278947 0.98663369 0.95768630 0.73242172 0.07453237
      [73] 0.80108438 0.69636892 0.72937981 0.94516991 0.94742301 0.84287608
      [79] 0.92040705 0.96737853 0.85617290 0.25305094 0.05387566 0.46639136
      [85] 0.52381805

