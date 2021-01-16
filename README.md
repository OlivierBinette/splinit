
<!-- README.md is generated from README.Rmd. Please edit that file -->

# splinit: Periodic spline regression and closed curve reconstruction

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("OlivierBinette/splinit")
```

## Example

Get a sample from a curve.

``` r
curve <- function(u) {
  cbind(2*sin(u+1)*cos(2*u) + sin(u), cos(u)+cos(u-2.3))
}
pts = curve(2*pi*(1:100)/100) + rnorm(0, 0.02, n=200)
```

Find a closed curve approximation.

``` r
library(splinit)

pretty::plot(pts, xlab="", ylab="")
pretty::lines(splinit(pts))
#> Warning: executing %dopar% sequentially: no parallel backend registered
```

![](man/figures/README-unnamed-chunk-4-1.png)<!-- -->
