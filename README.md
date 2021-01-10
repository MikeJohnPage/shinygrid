
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinygrid <img src='logo.png' align="right" height="150" /></a>

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

## Overview

Dealing with nested row and column grid structures in Shiny dashboards
can be tricky. shinygrid takes care of this by allowing you to visually
draw a grid structure in code, abstracting away the difficult details:

``` r
draw_grid(
  "sidebar box1 box2",
  "sidebar box1 box3",
  "sidebar box4 box3"
)
```

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MikeJohnPage/shinygrid")
```
