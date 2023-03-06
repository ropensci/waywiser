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
       1 local_getis_ord_g_pvalue standard      -0.957
       2 local_getis_ord_g_pvalue standard      -2.38 
       3 local_getis_ord_g_pvalue standard      -1.99 
       4 local_getis_ord_g_pvalue standard      -1.49 
       5 local_getis_ord_g_pvalue standard      -1.14 
       6 local_getis_ord_g_pvalue standard      -1.67 
       7 local_getis_ord_g_pvalue standard      -0.611
       8 local_getis_ord_g_pvalue standard      -1.51 
       9 local_getis_ord_g_pvalue standard       0.914
      10 local_getis_ord_g_pvalue standard      -2.66 
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
       [1] -0.95703092 -2.37651824 -1.98639646 -1.48968819 -1.14367160 -1.67323552
       [7] -0.61137668 -1.50818196  0.91427329 -2.65764707 -2.97705720 -1.64576290
      [13]  1.11898636 -2.74377999  0.69534224  0.53582541 -2.24299423 -0.29365852
      [19] -1.44997944 -1.18484327 -0.57289964  0.68854566  0.78838128 -1.75496419
      [25]  0.35167603 -0.55582930 -1.14462836 -3.10188658 -2.13015897 -0.94254934
      [31] -0.02501526 -2.21845155 -2.31953659 -0.74282124  1.96056765 -0.19905032
      [37] -1.54240839  0.12991456 -1.49551404  0.18537663 -1.28727282  1.07218415
      [43]  0.11447443 -1.14615937  1.01560728 -2.17148098 -0.86175908 -1.16266487
      [49]  1.91658932 -0.91784557 -2.82358346  0.80751259 -1.49357999 -1.36315657
      [55] -0.13924492 -1.38457331 -1.93818228 -0.48741095 -1.54114993 -2.02960623
      [61]  1.11865687 -0.91804202 -0.42231197 -1.85064102 -0.96708764 -0.91441456
      [67]  1.61204940 -1.05203389 -2.31363710 -1.54157992 -0.54973510  1.29432107
      [73]  0.36062846 -0.06983063  0.58870501 -1.38066088 -1.87391887 -1.74267635
      [79] -1.51231465 -2.50110732 -0.37511971  1.08576562  1.30781540  0.09706797
      [85]  1.04334354

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
       1 local_getis_ord_gstar_pvalue standard      -0.928
       2 local_getis_ord_gstar_pvalue standard      -2.42 
       3 local_getis_ord_gstar_pvalue standard      -2.01 
       4 local_getis_ord_gstar_pvalue standard       1.54 
       5 local_getis_ord_gstar_pvalue standard       1.13 
       6 local_getis_ord_gstar_pvalue standard       1.62 
       7 local_getis_ord_gstar_pvalue standard      -0.554
       8 local_getis_ord_gstar_pvalue standard       1.64 
       9 local_getis_ord_gstar_pvalue standard      -0.985
      10 local_getis_ord_gstar_pvalue standard       2.69 
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
       [1] -0.92816695 -2.42470555 -2.00784429  1.53617851  1.13260331  1.62400565
       [7] -0.55428038  1.64243825 -0.98539489  2.68565460  2.96519471  1.69652337
      [13] -1.16185753  3.16439057  0.64969706 -0.55240552 -2.19423075  0.27610554
      [19] -1.44501322 -1.21166757 -0.56648457  0.61190104 -0.79611540  1.63385167
      [25] -0.31861434 -0.44926186 -1.17501401  3.11280253  2.32865856  0.88042086
      [31] -0.13194511  2.21354187 -2.17574731 -0.60844480 -1.96787820  0.19727320
      [37] -1.55548255 -0.09750683 -1.54682679  0.24957740  1.17811150 -1.06630871
      [43] -0.12417327  1.15998734  0.96195330  2.37589498 -0.79159549 -1.25603214
      [49] -1.96434622 -0.83219779 -2.81831453  0.73280632 -1.45440285 -1.38493745
      [55]  0.05996567 -1.34737106 -1.81053829 -0.43772704 -1.56504095 -2.09800594
      [61] -1.15071197  1.03366539  0.49058751  1.79173555  0.94769387  0.74577429
      [67] -1.65835787 -1.02402307 -2.33021240 -1.69185894  0.52329971 -1.33697468
      [73]  0.28084996  0.01574376 -0.58590584 -1.49229603  1.91872493  1.96074053
      [79]  1.66725553  2.58550604 -0.35745846 -1.16131159 -1.39785866  0.04240485
      [85] -1.00545023

