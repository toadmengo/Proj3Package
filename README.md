
# Proj3Package

<!-- badges: start -->
[![R-CMD-check](https://github.com/toadmengo/Proj3Package/workflows/R-CMD-check/badge.svg)](https://github.com/toadmengo/Proj3Package/actions)
[![codecov](https://codecov.io/gh/toadmengo/Proj3Package/branch/master/graph/badge.svg?token=0KPDTJDOLB)](https://codecov.io/gh/toadmengo/Proj3Package)
<!-- badges: end -->

The goal of Proj3Package is to ...

## Installation

Install directly from Github using the `devtools` package.

``` r
#install.packages("devtools")
devtools::install_github("toadmengo/Proj3Package", build_vignettes = TRUE, build_opts = c())
library(Proj3Package)
```

## Use

The vignettes contains example usage of the functions in the package. To use the vignette, use the following code.

```r
library(Proj3Package)
# Use this to view the vignette in the Proj3Package HTML help
help(package = "Proj3Package", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "Proj3Package")
```


