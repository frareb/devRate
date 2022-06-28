
<!-- README.md is generated from README.Rmd. Please edit that file -->

# devRate package

[![Build
Status](https://travis-ci.org/frareb/devRate.svg?branch=master)](https://travis-ci.org/frareb/devRate)
[![CRAN
version](https://www.r-pkg.org/badges/version/devRate)](https://CRAN.R-project.org/package=devRate)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/devRate)](https://CRAN.R-project.org/package=devRate)
[![Coverage
Status](https://img.shields.io/codecov/c/gh/frareb/devRate/master.svg)](https://codecov.io/gh/frareb/devRate?branch=master)

## Purpose of the package

The devRate package allows quantifying the relationship between
development rate and temperature in ectotherm organisms.

## How to use the package

You can use the package:

1.  to get development rate curves as a function of temperature for a
    specific organism (hundred of examples from the literature are
    included in the package);

2.  to know which equations exists and which are most used in the
    literature; and

3.  to relate development rate with temperature from your empirical
    data, using the equations from the package database.

## Installation instructions

``` r
# from CRAN
install.packages("devRate")

# Or the development version from GitHub:
# install.packages("devtools")
devtools::install_github("frareb/devRate")
```

## Overview

The devRate package provides three categories of functions:

1.  functions to find development rate information about a specific
    organism (Order, Family, Genus, species)

2.  functions to relate development rate and temperature, and

3.  functions to plot your empirical datasets and the associated fitted
    model, and/or to plot development curves from the literature.

## Author’s affiliation

UMR EGCE (IRD, CNRS, Univ. Paris-Saclay), Gif-sur-Yvette, France

To cite this package please use:

``` r
citation("devRate") 
#> 
#> To cite package 'devRate' in publications use:
#> 
#>   Rebaudo F, Regnier B (2022). _devRate: Quantify the Relationship
#>   Between Development Rate and Temperature in Ectotherms_. R package
#>   version 0.2.2, <https://CRAN.R-project.org/package=devRate>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {devRate: Quantify the Relationship Between Development Rate and Temperature in Ectotherms},
#>     author = {Francois Rebaudo and Baptiste Regnier},
#>     year = {2022},
#>     note = {R package version 0.2.2},
#>     url = {https://CRAN.R-project.org/package=devRate},
#>   }
```

and/or: Rebaudo F., Struelens Q., Dangles O. Modelling
temperature-dependent development rate and phenology in arthropods: The
devRate package for R. *Methods Ecol Evol.* 2017;00:1-7.
<https://doi.org/10.1111/2041-210X.12935>
