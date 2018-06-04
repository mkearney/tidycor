
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidycor <img src="man/figures/logo.png" width="160px" align="right" />
======================================================================

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

🎓 Tidy tools for academics

\*\*\* This package is in very early development. Feedback is encouraged!!! \*\*\*
----------------------------------------------------------------------------------

Installation
------------

<!-- You can install the released version of tidycor from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("tidycor")
```
-->
Install the development version from [Github](https://github.com/mkearney/tidycor) with:

``` r
## install devtools if not already
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
## install tidycor from Github
devtools::install_github("mkearney/tidycor")
```

Load the package (it, of course, plays nicely with tidyverse).

``` r
## load tidyverse
library(tidyverse)

## load tidycor
library(tidycor)
```

Reliability
-----------

Estimate Cronbach's alpha for a set of variables.

``` r
## reliability of social media use items
cronbachs_alpha(polcom, ambiv_sexism_1:ambiv_sexism_6)
#>                           items    alpha alpha.std
#> 1 ambiv_sexism_1:ambiv_sexism_6 0.904609  0.904600
#> 2               -ambiv_sexism_1 0.882322  0.882225
#> 3               -ambiv_sexism_2 0.884272  0.884121
#> 4               -ambiv_sexism_3 0.896061  0.896218
#> 5               -ambiv_sexism_4 0.897127  0.897411
#> 6               -ambiv_sexism_5 0.883554  0.883420
#> 7               -ambiv_sexism_6 0.881595  0.881855
```
