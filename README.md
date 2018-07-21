---
output: github_document
---
[![pipeline status](https://gitlab.com/fvafrCU/packager/badges/master/pipeline.svg)](https://gitlab.com/fvafrCU/packager/commits/master)    
[![coverage report](https://gitlab.com/fvafrCU/packager/badges/master/coverage.svg)](https://gitlab.com/fvafrCU/packager/commits/master)
<!-- 
    [![Build Status](https://travis-ci.org/fvafrCU/packager.svg?branch=master)](https://travis-ci.org/fvafrCU/packager)
    [![Coverage Status](https://codecov.io/github/fvafrCU/packager/coverage.svg?branch=master)](https://codecov.io/github/fvafrCU/packager?branch=master)
-->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/packager)](https://cran.r-project.org/package=packager)
[![RStudio_downloads_monthly](https://cranlogs.r-pkg.org/badges/packager)](https://cran.r-project.org/package=packager)
[![RStudio_downloads_total](https://cranlogs.r-pkg.org/badges/grand-total/packager)](https://cran.r-project.org/package=packager)

<!-- README.md is generated from README.Rmd. Please edit that file -->



# packager
## Introduction
Please read the
[vignette](https://fvafrCU.gitlab.io/packager/inst/doc/An_Introduction_to_packager.html).

Or, after installation, the help page:

```r
help("packager-package", package = "packager")
```

```
#> Helps Me Build Packages
#> 
#> Description:
#> 
#>      Helper functions for a build system, heavily borrowing from and
#>      extending package `devtools 1.13.3`.
#> 
#> Details:
#> 
#>      I find devtools very helpful but it lacks some functionality I
#>      would want while creating and building a package.  Maybe
#>      'packager' should be two packages.
```

## Installation

You can install packager from github with:


```r
if (! require("devtools")) install.packages("devtools")
devtools::install_git("https://gitlab.com/fvafrCU/packager")
```


