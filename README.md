
<!-- README.md is generated from README.Rmd. Please edit that file -->

# waywiser <a href="https://mikemahoney218.github.io/waywiser/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/mikemahoney218/waywiser/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mikemahoney218/waywiser/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/waywiser)](https://CRAN.R-project.org/package=waywiser)
[![Codecov test
coverage](https://codecov.io/gh/mikemahoney218/waywiser/branch/main/graph/badge.svg)](https://app.codecov.io/gh/mikemahoney218/waywiser?branch=main)
<!-- badges: end -->

“Waywiser” is an old-timey name for a [surveyor’s
wheel](https://en.wikipedia.org/wiki/Surveyor%27s_wheel), a device that
makes measuring long distances easier than with measurement tools like a
ruler or yardstick. The waywiser R package makes measuring model
performance on spatial data easier, extending the
[yardstick](https://yardstick.tidymodels.org/) R package to incorporate
measures of spatial autocorrelation provided by
[sfdep](https://sfdep.josiahparry.com/).

Please note that this package is highly experimental. The user-facing
API is likely to change without deprecation warnings up until the first
CRAN release.

## Installation

You can install the development version of waywiser from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mikemahoney218/waywiser")
```

## Example

Let’s walk through how we can use waywiser to find local indicators of
spatial autocorrelation for a very simple model, looking at how tree
canopy coverage impacts temperature in Boston, Massachusetts. First
things first, let’s load a few libraries:

``` r
# waywiser itself, of course:
library(waywiser)
# For our data set:
library(spatialsample)
# For the %>% pipe and mutate:
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
# For dealing with spatial data:
library(sf)
#> Linking to GEOS 3.10.2, GDAL 3.4.3, PROJ 8.2.0; sf_use_s2() is TRUE
```

We’ll be working with the `boston_canopy` data from the spatialsample
package, fitting a linear model to associate tree canopy coverage with
mean temperature. Let’s turn on sf’s ability to download coordinate
reference systems over the internet, just in case we don’t have the CRS
for that data yet:

``` r
sf_proj_network(TRUE)
#> [1] "https://cdn.proj.org"
sf_add_proj_units()
```

waywiser builds on top of the [sfdep](https://sfdep.josiahparry.com/)
package, itself an extension for the
[spdep](https://r-spatial.github.io/spdep/index.html) spatial dependence
library. These packages expect you to have objects representing the
neighbors of your data set and the spatial weights between observations.

As a result, right now waywiser also expects you to already have those
objects pre-created and to pass them as arguments to model metric
functions. We’ll find the neighbors in our data using the
`st_contiguity()` function, and then calculate spatial weights via
`st_weights()`:

``` r
nb <- st_contiguity(boston_canopy)
wt <- st_weights(nb)
```

With our spatial relationships defined, we can now fit a model and
calculate spatial dependency in our model residuals!

We’ll fit a simple linear model relating canopy coverage to daily
temperatures, and then generate predictions from that model. We can use
`ww_local_moran_i()` to calculate the local spatial autocorrelation of
our residuals at each data point:

``` r
boston_canopy %>%
  mutate(pred = predict(lm(mean_temp ~ canopy_gain, .))) %>% 
  ww_local_moran_i(mean_temp, pred, nb, wt)
#> # A tibble: 682 × 4
#>    .metric       .estimator .estimate                                   geometry
#>    <chr>         <chr>          <dbl>            <MULTIPOLYGON [US_survey_foot]>
#>  1 local_moran_i standard     0.172   (((745307.9 2921698, 745307.9 2921699, 74…
#>  2 local_moran_i standard     0.149   (((745307.9 2921698, 745307.9 2921699, 74…
#>  3 local_moran_i standard    16.4     (((745307.9 2921698, 745307.9 2921699, 74…
#>  4 local_moran_i standard     0.0434  (((745307.9 2921698, 745307.9 2921699, 74…
#>  5 local_moran_i standard     0.270   (((745307.9 2921698, 745307.9 2921699, 74…
#>  6 local_moran_i standard     0.00428 (((745307.9 2921698, 745307.9 2921699, 74…
#>  7 local_moran_i standard     0.247   (((745307.9 2921698, 745307.9 2921699, 74…
#>  8 local_moran_i standard     0.00144 (((745307.9 2921698, 745307.9 2921699, 74…
#>  9 local_moran_i standard     0.0619  (((745307.9 2921698, 745307.9 2921699, 74…
#> 10 local_moran_i standard    -0.00195 (((745307.9 2921698, 745307.9 2921699, 74…
#> # … with 672 more rows
```

Or if we use `ww_local_moran_i_vec`, we can add a column to our original
data frame with our statistic, which makes plotting using our original
geometries easier:

``` r
library(ggplot2)

boston_canopy %>%
  mutate(pred = predict(lm(mean_temp ~ canopy_gain, .)),
         .estimate = ww_local_moran_i_vec(mean_temp, pred, nb, wt)) %>% 
  mutate(
    cut_points = case_when(
      .estimate <= -1 ~ "(-Inf, -1]",
      .estimate <= -0.5 ~ "(-1, -0.5]",
      .estimate <= 0 ~ "(-0.5, 0]",
      .estimate <= 0.5 ~ "(0, 0.5]",
      .estimate <= 1 ~ "(0.5, 1]",
      .estimate > 1 ~ "(1, Inf)",
    ),
    cut_points = factor(
      cut_points,
      rev(
        c(
          "(-Inf, -1]", 
          "(-1, -0.5]", 
          "(-0.5, 0]", 
          "(0, 0.5]", 
          "(0.5, 1]", 
          "(1, Inf)")
      )
    )
  ) %>% 
  st_as_sf() %>% 
  ggplot(aes(fill = cut_points)) +
  geom_sf() + 
  scale_fill_brewer(palette = "BrBG")
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

This makes it easy to see what areas are poorly represented by our
model, which might lead us to identify ways to improve our model or help
us identify caveats and limitations of the models we’re working with.

## Contributing

This project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

-   For questions and discussions about tidymodels packages, modeling,
    and machine learning, please [post on RStudio
    Community](https://community.rstudio.com/new-topic?category_id=15&tags=tidymodels,question).

-   If you think you have encountered a bug, please [submit an
    issue](https://github.com/tidymodels/rules/issues).

-   Either way, learn how to create and share a
    [reprex](https://reprex.tidyverse.org/articles/articles/learn-reprex.html)
    (a minimal, reproducible example), to clearly communicate about your
    code.
