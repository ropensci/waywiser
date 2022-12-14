# Local Getis-Ord statistics are stable

    Code
      df_local_i <- ww_local_getis_ord_g(guerry_modeled, Crm_prs, predictions)
      df_local_i[1:3]
    Output
      # A tibble: 85 x 3
         .metric           .estimator .estimate
         <chr>             <chr>          <dbl>
       1 local_getis_ord_g standard       0.913
       2 local_getis_ord_g standard       2.49 
       3 local_getis_ord_g standard       2.15 
       4 local_getis_ord_g standard      -1.58 
       5 local_getis_ord_g standard      -1.19 
       6 local_getis_ord_g standard      -1.68 
       7 local_getis_ord_g standard       0.627
       8 local_getis_ord_g standard      -1.60 
       9 local_getis_ord_g standard       0.964
      10 local_getis_ord_g standard      -2.71 
      # ... with 75 more rows

---

    Code
      df_local_i_p <- ww_local_getis_ord_g_pvalue(guerry_modeled, Crm_prs,
        predictions)
      df_local_i_p[1:3]
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
      (vec_local_i <- ww_local_getis_ord_g_vec(guerry_modeled$Crm_prs, guerry_modeled$
        predictions, weights))
    Output
       [1]  0.91288164  2.49305353  2.14692501 -1.57511235 -1.18988831 -1.67703329
       [7]  0.62706195 -1.60139345  0.96397088 -2.71475331 -3.05125861 -1.64429475
      [13]  1.15584938 -2.90161984 -0.63376101  0.61005430  2.29888368 -0.28799789
      [19]  1.53012357  1.22699107  0.59816133 -0.65331166  0.87501663 -1.75661589
      [25]  0.39100454  0.52017062  1.16424138 -3.20781683 -2.29583912 -0.98116177
      [31]  0.06685550 -2.13635236  2.30311768  0.63548278  1.99855789 -0.24366435
      [37]  1.55058791  0.17231008  1.52062197 -0.22999799 -1.29501163  1.08298736
      [43]  0.13063616 -1.21798455 -1.00549332 -2.29306942  0.89419782  1.19832028
      [49]  2.03154442  0.87398122  2.88484032 -0.76194353  1.52453019  1.36764146
      [55] -0.06618369  1.47010332  1.89277905  0.52529402  1.63007385  2.00852739
      [61]  1.14763247 -0.91644996 -0.49669001 -1.85515687 -1.00553207 -0.86081555
      [67]  1.63219209  1.04953518  2.51838763  1.71259713 -0.49967695  1.35387348
      [73] -0.33336379 -0.07498653  0.56058846  1.54116260 -1.91772217 -1.92601432
      [79] -1.62505600 -2.60384397  0.42262985  1.16021704  1.46229964  0.06056077
      [85]  1.08651166

---

    Code
      (vec_local_i_p <- ww_local_getis_ord_g_pvalue_vec(guerry_modeled$Crm_prs,
      guerry_modeled$predictions, weights))
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
      df_local_i <- ww_local_getis_ord_g(guerry_modeled, Crm_prs, predictions,
        weights)
      df_local_i[1:3]
    Output
      # A tibble: 85 x 3
         .metric               .estimator .estimate
         <chr>                 <chr>          <dbl>
       1 local_getis_ord_gstar standard       1.35 
       2 local_getis_ord_gstar standard       2.64 
       3 local_getis_ord_gstar standard       2.33 
       4 local_getis_ord_gstar standard      -1.84 
       5 local_getis_ord_gstar standard      -1.19 
       6 local_getis_ord_gstar standard      -2.06 
       7 local_getis_ord_gstar standard       1.58 
       8 local_getis_ord_gstar standard      -2.32 
       9 local_getis_ord_gstar standard       0.880
      10 local_getis_ord_gstar standard      -2.74 
      # ... with 75 more rows

---

    Code
      df_local_i_p <- ww_local_getis_ord_g_pvalue(guerry_modeled, Crm_prs,
        predictions, weights)
      df_local_i_p[1:3]
    Output
      # A tibble: 85 x 3
         .metric                      .estimator .estimate
         <chr>                        <chr>          <dbl>
       1 local_getis_ord_gstar_pvalue standard     0.156  
       2 local_getis_ord_gstar_pvalue standard     0.0116 
       3 local_getis_ord_gstar_pvalue standard     0.0344 
       4 local_getis_ord_gstar_pvalue standard     0.0673 
       5 local_getis_ord_gstar_pvalue standard     0.237  
       6 local_getis_ord_gstar_pvalue standard     0.0381 
       7 local_getis_ord_gstar_pvalue standard     0.111  
       8 local_getis_ord_gstar_pvalue standard     0.0150 
       9 local_getis_ord_gstar_pvalue standard     0.358  
      10 local_getis_ord_gstar_pvalue standard     0.00806
      # ... with 75 more rows

---

    Code
      (vec_local_i <- ww_local_getis_ord_g_vec(guerry_modeled$Crm_prs, guerry_modeled$
        predictions, weights))
    Output
       [1]  1.35371776  2.64470358  2.33101218 -1.83696218 -1.19214894 -2.06145107
       [7]  1.58230958 -2.31764702  0.88028873 -2.74035690 -3.39292895 -1.87812026
      [13]  0.85856419 -2.81583254 -0.31227470  0.49524512  2.24081985 -0.51676147
      [19]  2.00481255  1.63825389  1.39112875 -0.55103244  0.08003347 -1.97182396
      [25]  0.11106013  0.56430623  1.71460247 -3.33281642 -2.22026799 -0.98578659
      [31]  0.30960568 -2.34956428  2.25994512  1.20029086  1.79507717 -0.29168508
      [37]  1.78306056  0.01327693  1.48600121  0.13091399 -1.40966303  0.93339994
      [43]  0.02027691 -1.82812173 -0.79364697 -2.75575017  1.26343283  1.74008875
      [49]  1.69116164  1.14388681  3.09199200 -0.26702307  1.66914599  1.42530173
      [55] -0.65613591  1.62256416  2.11228076  0.90733590  1.93759338  1.91124325
      [61]  0.92968354 -1.01687632 -0.94802983 -2.54791742 -1.35286802 -1.66942606
      [67]  1.40993406  1.10799183  2.75546130  2.26572376 -0.86272825  1.07976560
      [73] -0.21434998 -0.43824366  0.43342398  2.13948993 -2.14931849 -2.06450852
      [79] -1.88711642 -2.74970443  0.42882579  0.82915623  1.17030359  0.01011938
      [85]  0.90033560

---

    Code
      (vec_local_i_p <- ww_local_getis_ord_g_pvalue_vec(guerry_modeled$Crm_prs,
      guerry_modeled$predictions, weights))
    Output
       [1] 0.1556270646 0.0116447985 0.0343579054 0.0672754947 0.2371234223
       [6] 0.0381479713 0.1108946596 0.0149709233 0.3578951630 0.0080552150
      [11] 0.0007852371 0.0632757613 0.4155364917 0.0018566935 0.7748476149
      [16] 0.6703539478 0.0383894035 0.6078981653 0.0510351305 0.0908735217
      [21] 0.1605734984 0.6215092414 0.9515965060 0.0611221274 0.9740911085
      [26] 0.5902312324 0.0632866881 0.0010014584 0.0232834875 0.3722611128
      [31] 0.7079512526 0.0160720461 0.0333995324 0.2343415826 0.0920927654
      [36] 0.7876002799 0.0744825253 0.9279237088 0.1415173480 0.9105761435
      [41] 0.2014059458 0.3790312576 0.9827714916 0.0610703741 0.4812254025
      [46] 0.0038315923 0.2254361953 0.0702185668 0.1167672206 0.2575250718
      [51] 0.0025167455 0.8330098136 0.1081071310 0.1524809981 0.4978473731
      [56] 0.1345782976 0.0448982231 0.3929008371 0.0554530884 0.0629719885
      [61] 0.3952931115 0.2720290994 0.3470507086 0.0131916927 0.2057211749
      [66] 0.0964464799 0.1585356769 0.2943535581 0.0110636471 0.0169802716
      [71] 0.3753718675 0.2844121482 0.8707429756 0.6780737931 0.6536939343
      [76] 0.0321287255 0.0348083487 0.0369507015 0.0469535215 0.0070540349
      [81] 0.7239024648 0.3958549621 0.2822598709 0.9346564649 0.4346557937

