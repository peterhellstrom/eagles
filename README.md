
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eagles

<!-- badges: start -->
<!-- badges: end -->

The goal of eagles is to …

## Installation

You can install the development version of eagles from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("peterhellstrom/eagles")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(eagles)
#> Loading required package: tidyverse
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.2     ✔ readr     2.1.4
#> ✔ forcats   1.0.0     ✔ stringr   1.5.0
#> ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
#> ✔ purrr     1.0.1     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
#> Loading required package: janitor
#> 
#> 
#> Attaching package: 'janitor'
#> 
#> 
#> The following objects are masked from 'package:stats':
#> 
#>     chisq.test, fisher.test
#> 
#> 
#> Loading required package: glue
#> 
#> Loading required package: sf
#> 
#> Linking to GEOS 3.11.2, GDAL 3.6.2, PROJ 9.2.0; sf_use_s2() is TRUE
#> 
#> Loading required package: leaflet
#> 
#> Loading required package: leaflet.extras
#> 
#> Loading required package: mapview
#> 
#> The legacy packages maptools, rgdal, and rgeos, underpinning this package
#> will retire shortly. Please refer to R-spatial evolution reports on
#> https://r-spatial.org/r/2023/05/15/evolution4.html for details.
#> This package is now running under evolution status 0 
#> 
#> Loading required package: readxl
#> 
#> Loading required package: writexl
#> 
#> 
#> Attaching package: 'eagles'
#> 
#> 
#> The following object is masked from 'package:sf':
#> 
#>     st_filter
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
