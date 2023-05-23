metaboWorkflows
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/jasenfinch/metaboWorkflows/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jasenfinch/metaboWorkflows/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/jasenfinch/metaboWorkflows/branch/master/graph/badge.svg)](https://codecov.io/gh/jasenfinch/metaboWorkflows?branch=master)
[![license](https://img.shields.io/badge/license-GNU%20GPL%20v3.0-blue.svg)](https://github.com/jasenfinch/metaboWorkflows/blob/master/DESCRIPTION)
[![GitHub
release](https://img.shields.io/github/release/jasenfinch/metaboWorkflows.svg)](https://GitHub.com/jasenfinch/metaboWorkflows/releases/)
<!-- badges: end -->

> Workflow Project Templates for Metabolomics Analyses

## Overview

This package provides reproducible workflow project templates for
metabolomics analyses using the [hrm](https://jasenfinch.github.io/hrm/)
R package ecosystem.

These project templates utilise a number of tools to promote efficient
and reproducible analysis, agnostic of the actual analysis R code. These
tools include:

  - [targets](https://docs.ropensci.org/targets/) - an R focused
    pipeline toolkit for efficiently maintaining reproducible analysis
    workflows.
  - [renv](https://rstudio.github.io/renv/) - an R package for
    project-local R package dependency management for maintaining
    reproducible R package environments.
  - [git](https://git-scm.com/) - a widely used, open-source distributed
    version control system.
  - [docker](https://www.docker.com/) - enables the containerization of
    operating system (OS) level environments. This can be used to define
    reproducible OS environments in which a workflow analysis can be
    performed.

Project templates are available for the following metabolomic
techniques:

  - FIE-HRMS fingerprinting
  - NSI-HRMS fingerprinting
  - RP-LC-HRMS profiling
  - NP-LC-HRMS profiling
  - GC-MS profiling

## Installation

The `metaboWorkflows` package can be installed from GitHub using the
following:

``` r
remotes::install_github('jasenfinch/metaboWorkflows',build_vignettes = TRUE)
```

## Learn more

The package documentation can be browsed online at
<https://jasenfinch.github.io/metaboWorkflows/>.

If this is your first time using `metaboWorkflows` see the
[Introduction](https://jasenfinch.github.io/metaboWorkflows/articles/metaboWorkflows.html)
vignette for information on how to get started.

If you believe you’ve found a bug in `metaboWorkflows`, please file a
bug (and, if possible, a [reproducible
example](https://reprex.tidyverse.org)) at
<https://github.com/jasenfinch/metaboWorkflows/issues>.
