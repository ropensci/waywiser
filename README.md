
<!-- README.md is generated from README.Rmd. Please edit that file -->

# waywiserrr <a href="https://docs.ropensci.org/waywiser/"><img src="man/figures/logo.png" align="right" height="138" /></a>

waywiserrr is [waywise**r**](https://docs.ropensci.org/waywiser/),
**r**ewritten in **R**ust. I’m using this repo to experiment with using
Rust in R packages, and to try and muddle my way through learning
ndarray.

You probably shouldn’t use this; this repo forked from waywiser a while
back, with most bug fixes and feature additions put into the main repo.
I’m also not particularly likely to port any of this code into the main
repo, as the speedups don’t yet justify the headache of maintaining a
package with compiled code (or, in particular, maintaining a package
with Rust code).

## Comparisons

``` r
benchmarks <- bench::press(
  rows = 10^(2:6),
  {
    df <- withr::with_seed(
      1107,
      data.frame(x = rnorm(rows), y = rnorm(rows))
    )
    bench::mark(
      waywiser::ww_agreement_coefficient(df, x, y),
      waywiserrr::ww_agreement_coefficient(df, x, y),
      waywiser::ww_systematic_agreement_coefficient(df, x, y),
      waywiserrr::ww_systematic_agreement_coefficient(df, x, y),
      waywiser::ww_unsystematic_agreement_coefficient(df, x, y),
      waywiserrr::ww_unsystematic_agreement_coefficient(df, x, y),
      waywiser::ww_unsystematic_mpd(df, x, y),
      waywiserrr::ww_unsystematic_mpd(df, x, y),
      waywiser::ww_systematic_mpd(df, x, y),
      waywiserrr::ww_systematic_mpd(df, x, y),
      waywiser::ww_unsystematic_rmpd(df, x, y),
      waywiserrr::ww_unsystematic_rmpd(df, x, y),
      waywiser::ww_systematic_rmpd(df, x, y),
      waywiserrr::ww_systematic_rmpd(df, x, y),
      waywiser::ww_willmott_d(df, x, y),
      waywiserrr::ww_willmott_d(df, x, y),
      waywiser::ww_willmott_d1(df, x, y),
      waywiserrr::ww_willmott_d1(df, x, y),
      waywiser::ww_willmott_dr(df, x, y),
      waywiserrr::ww_willmott_dr(df, x, y),
      waywiser::ww_systematic_mse(df, x, y),
      waywiserrr::ww_systematic_mse(df, x, y),
      waywiser::ww_unsystematic_mse(df, x, y),
      waywiserrr::ww_unsystematic_mse(df, x, y),
      waywiser::ww_unsystematic_rmse(df, x, y),
      waywiserrr::ww_unsystematic_rmse(df, x, y),
      check = FALSE,
      min_iterations = 10,
      filter_gc = FALSE
    )
  }
)

library(ggplot2)
benchmarks |> 
  dplyr::mutate(
    expression = as.character(expression),
    median = as.numeric(median)
  ) |> 
  tidyr::separate_wider_delim(
    "expression", "::", names = c("package", "func")
  ) |> 
  dplyr::arrange(rows, func, package) |> 
  dplyr::group_by(rows, func) |> 
  dplyr::summarise(
    ratio = median[1] / median[2],
    .groups = "drop"
  ) |> 
  ggplot() +
  aes(x = rows, y = ratio) + 
  geom_abline(slope = 0, intercept = 1, linesize = 0.2) +
  geom_line(linewidth = .5) +
  geom_point(aes(color = ratio > 1)) +
  scale_x_log10() +
  scale_color_manual(
    "Runs faster in:",
    labels = c("waywiser", "waywiserrr"),
    values = c("#c5784f", "#588d75")
  ) +
  labs(
    x = "Number of rows in training data",
    y = "Runtime ratio (Base R / Rust)",
    title = "Changes in speed, waywiser vs waywiserrr",
    subtitle = "Positive values run faster in waywiserrr, negative slower"
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(face = "italic"),
    panel.grid = element_blank(),
    strip.text = element_text(size = 7),
    axis.text.x = element_text(size = 7)
  ) +
  facet_wrap(~ func)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r

print(
  benchmarks[c("expression", "rows", "median")],
  n = 999
)
#> # A tibble: 130 × 3
#>     expression                                                     rows   median
#>     <bch:expr>                                                    <dbl> <bch:tm>
#>   1 waywiser::ww_agreement_coefficient(df, x, y)                    100   4.84ms
#>   2 waywiserrr::ww_agreement_coefficient(df, x, y)                  100   4.39ms
#>   3 waywiser::ww_systematic_agreement_coefficient(df, x, y)         100   4.39ms
#>   4 waywiserrr::ww_systematic_agreement_coefficient(df, x, y)       100   3.95ms
#>   5 waywiser::ww_unsystematic_agreement_coefficient(df, x, y)       100   2.55ms
#>   6 waywiserrr::ww_unsystematic_agreement_coefficient(df, x, y)     100   2.35ms
#>   7 waywiser::ww_unsystematic_mpd(df, x, y)                         100   2.34ms
#>   8 waywiserrr::ww_unsystematic_mpd(df, x, y)                       100   2.22ms
#>   9 waywiser::ww_systematic_mpd(df, x, y)                           100   2.19ms
#>  10 waywiserrr::ww_systematic_mpd(df, x, y)                         100   2.12ms
#>  11 waywiser::ww_unsystematic_rmpd(df, x, y)                        100    2.2ms
#>  12 waywiserrr::ww_unsystematic_rmpd(df, x, y)                      100   2.42ms
#>  13 waywiser::ww_systematic_rmpd(df, x, y)                          100   2.52ms
#>  14 waywiserrr::ww_systematic_rmpd(df, x, y)                        100   2.55ms
#>  15 waywiser::ww_willmott_d(df, x, y)                               100   3.22ms
#>  16 waywiserrr::ww_willmott_d(df, x, y)                             100   4.67ms
#>  17 waywiser::ww_willmott_d1(df, x, y)                              100   4.37ms
#>  18 waywiserrr::ww_willmott_d1(df, x, y)                            100   4.93ms
#>  19 waywiser::ww_willmott_dr(df, x, y)                              100   4.46ms
#>  20 waywiserrr::ww_willmott_dr(df, x, y)                            100   5.03ms
#>  21 waywiser::ww_systematic_mse(df, x, y)                           100   6.86ms
#>  22 waywiserrr::ww_systematic_mse(df, x, y)                         100    6.5ms
#>  23 waywiser::ww_unsystematic_mse(df, x, y)                         100   5.91ms
#>  24 waywiserrr::ww_unsystematic_mse(df, x, y)                       100   5.56ms
#>  25 waywiser::ww_unsystematic_rmse(df, x, y)                        100   3.26ms
#>  26 waywiserrr::ww_unsystematic_rmse(df, x, y)                      100   3.06ms
#>  27 waywiser::ww_agreement_coefficient(df, x, y)                   1000   2.23ms
#>  28 waywiserrr::ww_agreement_coefficient(df, x, y)                 1000   2.14ms
#>  29 waywiser::ww_systematic_agreement_coefficient(df, x, y)        1000   2.22ms
#>  30 waywiserrr::ww_systematic_agreement_coefficient(df, x, y)      1000   2.11ms
#>  31 waywiser::ww_unsystematic_agreement_coefficient(df, x, y)      1000    2.3ms
#>  32 waywiserrr::ww_unsystematic_agreement_coefficient(df, x, y)    1000   2.44ms
#>  33 waywiser::ww_unsystematic_mpd(df, x, y)                        1000   3.25ms
#>  34 waywiserrr::ww_unsystematic_mpd(df, x, y)                      1000    2.4ms
#>  35 waywiser::ww_systematic_mpd(df, x, y)                          1000   2.73ms
#>  36 waywiserrr::ww_systematic_mpd(df, x, y)                        1000    3.2ms
#>  37 waywiser::ww_unsystematic_rmpd(df, x, y)                       1000   4.08ms
#>  38 waywiserrr::ww_unsystematic_rmpd(df, x, y)                     1000   4.11ms
#>  39 waywiser::ww_systematic_rmpd(df, x, y)                         1000   4.31ms
#>  40 waywiserrr::ww_systematic_rmpd(df, x, y)                       1000   3.95ms
#>  41 waywiser::ww_willmott_d(df, x, y)                              1000   2.42ms
#>  42 waywiserrr::ww_willmott_d(df, x, y)                            1000   2.26ms
#>  43 waywiser::ww_willmott_d1(df, x, y)                             1000   2.24ms
#>  44 waywiserrr::ww_willmott_d1(df, x, y)                           1000   2.13ms
#>  45 waywiser::ww_willmott_dr(df, x, y)                             1000   2.09ms
#>  46 waywiserrr::ww_willmott_dr(df, x, y)                           1000   2.04ms
#>  47 waywiser::ww_systematic_mse(df, x, y)                          1000   2.73ms
#>  48 waywiserrr::ww_systematic_mse(df, x, y)                        1000   2.74ms
#>  49 waywiser::ww_unsystematic_mse(df, x, y)                        1000   2.83ms
#>  50 waywiserrr::ww_unsystematic_mse(df, x, y)                      1000   3.11ms
#>  51 waywiser::ww_unsystematic_rmse(df, x, y)                       1000   3.43ms
#>  52 waywiserrr::ww_unsystematic_rmse(df, x, y)                     1000    3.6ms
#>  53 waywiser::ww_agreement_coefficient(df, x, y)                  10000   4.75ms
#>  54 waywiserrr::ww_agreement_coefficient(df, x, y)                10000   3.67ms
#>  55 waywiser::ww_systematic_agreement_coefficient(df, x, y)       10000   5.79ms
#>  56 waywiserrr::ww_systematic_agreement_coefficient(df, x, y)     10000   4.84ms
#>  57 waywiser::ww_unsystematic_agreement_coefficient(df, x, y)     10000   5.78ms
#>  58 waywiserrr::ww_unsystematic_agreement_coefficient(df, x, y)   10000   4.18ms
#>  59 waywiser::ww_unsystematic_mpd(df, x, y)                       10000   5.12ms
#>  60 waywiserrr::ww_unsystematic_mpd(df, x, y)                     10000   4.26ms
#>  61 waywiser::ww_systematic_mpd(df, x, y)                         10000   4.59ms
#>  62 waywiserrr::ww_systematic_mpd(df, x, y)                       10000   2.44ms
#>  63 waywiser::ww_unsystematic_rmpd(df, x, y)                      10000   3.04ms
#>  64 waywiserrr::ww_unsystematic_rmpd(df, x, y)                    10000   2.42ms
#>  65 waywiser::ww_systematic_rmpd(df, x, y)                        10000   2.78ms
#>  66 waywiserrr::ww_systematic_rmpd(df, x, y)                      10000   2.25ms
#>  67 waywiser::ww_willmott_d(df, x, y)                             10000   2.25ms
#>  68 waywiserrr::ww_willmott_d(df, x, y)                           10000   2.32ms
#>  69 waywiser::ww_willmott_d1(df, x, y)                            10000   2.78ms
#>  70 waywiserrr::ww_willmott_d1(df, x, y)                          10000   2.66ms
#>  71 waywiser::ww_willmott_dr(df, x, y)                            10000   2.84ms
#>  72 waywiserrr::ww_willmott_dr(df, x, y)                          10000   2.81ms
#>  73 waywiser::ww_systematic_mse(df, x, y)                         10000   6.09ms
#>  74 waywiserrr::ww_systematic_mse(df, x, y)                       10000   6.66ms
#>  75 waywiser::ww_unsystematic_mse(df, x, y)                       10000   7.15ms
#>  76 waywiserrr::ww_unsystematic_mse(df, x, y)                     10000   9.09ms
#>  77 waywiser::ww_unsystematic_rmse(df, x, y)                      10000   8.38ms
#>  78 waywiserrr::ww_unsystematic_rmse(df, x, y)                    10000   7.52ms
#>  79 waywiser::ww_agreement_coefficient(df, x, y)                 100000    5.3ms
#>  80 waywiserrr::ww_agreement_coefficient(df, x, y)               100000   4.08ms
#>  81 waywiser::ww_systematic_agreement_coefficient(df, x, y)      100000  12.04ms
#>  82 waywiserrr::ww_systematic_agreement_coefficient(df, x, y)    100000   6.71ms
#>  83 waywiser::ww_unsystematic_agreement_coefficient(df, x, y)    100000  12.05ms
#>  84 waywiserrr::ww_unsystematic_agreement_coefficient(df, x, y)  100000   5.75ms
#>  85 waywiser::ww_unsystematic_mpd(df, x, y)                      100000   9.44ms
#>  86 waywiserrr::ww_unsystematic_mpd(df, x, y)                    100000   4.85ms
#>  87 waywiser::ww_systematic_mpd(df, x, y)                        100000   9.19ms
#>  88 waywiserrr::ww_systematic_mpd(df, x, y)                      100000   5.64ms
#>  89 waywiser::ww_unsystematic_rmpd(df, x, y)                     100000  10.21ms
#>  90 waywiserrr::ww_unsystematic_rmpd(df, x, y)                   100000   6.19ms
#>  91 waywiser::ww_systematic_rmpd(df, x, y)                       100000  12.73ms
#>  92 waywiserrr::ww_systematic_rmpd(df, x, y)                     100000   7.55ms
#>  93 waywiser::ww_willmott_d(df, x, y)                            100000   6.23ms
#>  94 waywiserrr::ww_willmott_d(df, x, y)                          100000   7.32ms
#>  95 waywiser::ww_willmott_d1(df, x, y)                           100000   9.21ms
#>  96 waywiserrr::ww_willmott_d1(df, x, y)                         100000   7.58ms
#>  97 waywiser::ww_willmott_dr(df, x, y)                           100000    6.6ms
#>  98 waywiserrr::ww_willmott_dr(df, x, y)                         100000   6.47ms
#>  99 waywiser::ww_systematic_mse(df, x, y)                        100000  32.56ms
#> 100 waywiserrr::ww_systematic_mse(df, x, y)                      100000  30.11ms
#> 101 waywiser::ww_unsystematic_mse(df, x, y)                      100000  30.44ms
#> 102 waywiserrr::ww_unsystematic_mse(df, x, y)                    100000  20.18ms
#> 103 waywiser::ww_unsystematic_rmse(df, x, y)                     100000  19.01ms
#> 104 waywiserrr::ww_unsystematic_rmse(df, x, y)                   100000  18.67ms
#> 105 waywiser::ww_agreement_coefficient(df, x, y)                1000000  46.62ms
#> 106 waywiserrr::ww_agreement_coefficient(df, x, y)              1000000  45.31ms
#> 107 waywiser::ww_systematic_agreement_coefficient(df, x, y)     1000000 137.86ms
#> 108 waywiserrr::ww_systematic_agreement_coefficient(df, x, y)   1000000  86.89ms
#> 109 waywiser::ww_unsystematic_agreement_coefficient(df, x, y)   1000000 117.05ms
#> 110 waywiserrr::ww_unsystematic_agreement_coefficient(df, x, y) 1000000   72.9ms
#> 111 waywiser::ww_unsystematic_mpd(df, x, y)                     1000000  85.67ms
#> 112 waywiserrr::ww_unsystematic_mpd(df, x, y)                   1000000  50.16ms
#> 113 waywiser::ww_systematic_mpd(df, x, y)                       1000000  82.77ms
#> 114 waywiserrr::ww_systematic_mpd(df, x, y)                     1000000  47.01ms
#> 115 waywiser::ww_unsystematic_rmpd(df, x, y)                    1000000  72.61ms
#> 116 waywiserrr::ww_unsystematic_rmpd(df, x, y)                  1000000  45.11ms
#> 117 waywiser::ww_systematic_rmpd(df, x, y)                      1000000  86.83ms
#> 118 waywiserrr::ww_systematic_rmpd(df, x, y)                    1000000  62.24ms
#> 119 waywiser::ww_willmott_d(df, x, y)                           1000000  38.21ms
#> 120 waywiserrr::ww_willmott_d(df, x, y)                         1000000  40.58ms
#> 121 waywiser::ww_willmott_d1(df, x, y)                          1000000  44.97ms
#> 122 waywiserrr::ww_willmott_d1(df, x, y)                        1000000  45.29ms
#> 123 waywiser::ww_willmott_dr(df, x, y)                          1000000  41.46ms
#> 124 waywiserrr::ww_willmott_dr(df, x, y)                        1000000  35.93ms
#> 125 waywiser::ww_systematic_mse(df, x, y)                       1000000 239.48ms
#> 126 waywiserrr::ww_systematic_mse(df, x, y)                     1000000 178.39ms
#> 127 waywiser::ww_unsystematic_mse(df, x, y)                     1000000 186.75ms
#> 128 waywiserrr::ww_unsystematic_mse(df, x, y)                   1000000 271.88ms
#> 129 waywiser::ww_unsystematic_rmse(df, x, y)                    1000000  297.6ms
#> 130 waywiserrr::ww_unsystematic_rmse(df, x, y)                  1000000 254.61ms

# waywiser::ww_area_of_applicability()
# # & predict
# 
# waywiser::ww_multi_scale()
# 
# waywiser::ww_global_moran_i()
# 
# waywiser::ww_global_moran_pvalue()
# 
# waywiser::ww_local_moran_i()
# 
# waywiser::ww_local_moran_pvalue()
# 
# waywiser::ww_global_geary_c()
# 
# waywiser::ww_global_geary_pvalue()
# 
# waywiser::ww_local_geary_c()
# 
# waywiser::ww_local_geary_pvalue()
# 
# waywiser::ww_local_getis_ord_g()
# 
# waywiser::ww_local_getis_ord_g_pvalue()
#
# waywiser::ww_build_neighbors()
# 
# waywiser::ww_build_weights()
# 
# waywiser::ww_make_point_neighbors()
# 
# waywiser::ww_make_polygon_neighbors()
```
