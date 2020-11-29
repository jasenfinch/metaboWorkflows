# metaboWorkflows

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build status](https://github.com/jasenfinch/metaboWorkflows/workflows/R-CMD-check/badge.svg)](https://github.com/jasenfinch/metaboWorkflows/actions)
[![Codecov test coverage](https://codecov.io/gh/jasenfinch/metaboWorkflows/branch/master/graph/badge.svg)](https://codecov.io/gh/jasenfinch/metaboWorkflows?branch=master)
[![license](https://img.shields.io/badge/license-GNU%20GPL%20v3.0-blue.svg)](https://github.com/jasenfinch/metaboWorkflows/blob/master/DESCRIPTION) 
<!-- badges: end -->

R workflows for metabolomics analyses based on the [hrm](https://jasenfinch.github.io/hrm/) package ecosystem.

## Installation

``` r
devtools::install_github('jasenfinch/metaboWorkflows',build_vignette = T)

vignette('metaboWorkflows-usage', package = 'metaboWorkflows')
```