
# project3part1package

<!-- badges: start -->
[![R-CMD-check](https://github.com/thomson3uw/project3part1package/workflows/R-CMD-check/badge.svg)](https://github.com/thomson3uw/project3part1package/actions)
[![codecov](https://codecov.io/gh/thomson3uw/project3part1package/branch/master/graph/badge.svg)](https://codecov.io/gh/thomson3uw/project3part1package)
<!-- badges: end -->

This is a package created for the final project of STAT 302. The package itself provides varios tools for developing and testing a small set of model types.

## Installation

To install project3part1package, use the code below:

``` r
devtools::install_github("thomson3uw/project3part1package")
library(project3part1package)
```

## Use

This package includes a vignette that outlines the usage of all of its functions. Use the following code to set up and view said vignette: 

``` r
devtools::install_github("thomson3uw/project3part1package", build_vignette = TRUE, build_opts = c(), force = TRUE)
library(project3part1package)
# Use the following to access the vignette via help
help(package = "project3part1package", help_type = "html")
# Use this to view the vignette as a seperate HTML file
utils::browseVignettes(package = "project3part1package")
```

