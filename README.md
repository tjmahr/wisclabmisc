
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wisclabmisc

<!-- badges: start -->

<!-- badges: end -->

The goal of wisclabmisc is to reuse analysis functions across projects.

## Installation

You can install the development version of wisclabmisc from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tjmahr/wisclabmisc")
```

## gamlss helpers

`mem_gamlss()` (memory gamlss) provides a drop-in replacement for the
`gamlss()` function.

``` r
library(wisclabmisc)
library(gamlss)
library(tidyverse)

data <- as.data.frame(nlme::Orthodont)
model <- mem_gamlss(distance ~ age, data = data)
#> GAMLSS-RS iteration 1: Global Deviance = 505.577 
#> GAMLSS-RS iteration 2: Global Deviance = 505.577
```

The only difference between `mem_gamlss()` and `gamlss()` is that the
modified version includes a bundle of data in `.user` that records the
original dataset, session information and the call used to fit the
model.

``` r
str(model$.user, max.level = 1)
#> List of 3
#>  $ data        :'data.frame':    108 obs. of  4 variables:
#>  $ session_info:List of 2
#>   ..- attr(*, "class")= chr "session_info"
#>  $ call        : language mem_gamlss(distance ~ age, data = data)
```

gamlss does not store the data with the model with the data, and we need
the dataset because centile prediction often fails without the dataset:

``` r
newdata <- distinct(data, age)
centiles.pred(
  model, 
  cent = c(25, 50, 75),
  xname = "age", 
  xvalues = newdata$age, 
  plot = FALSE
)
#> Error in data.frame(data, source = namelist): arguments imply differing number of rows: 4, 5
```

But including the original dataset works:

``` r
centiles.pred(
  model, 
  cent = c(25, 50, 75),
  xname = "age", 
  xvalues = newdata$age, 
  plot = FALSE,
  data = model$.user$data
)
#>    x       25       50       75
#> 1  8 20.34723 22.04259 23.73796
#> 2 10 21.66760 23.36296 25.05833
#> 3 12 22.98797 24.68333 26.37870
#> 4 14 24.30834 26.00370 27.69907
```

This package provides `predict_centiles()` as a streamlined version of
the above code, but:

  - assumes the model was fitted with `mem_gamlss()`
  - returns a tibble
  - keeps the predictor name (here, `age` instead of `x`)
  - prefixes the centiles with `q` (for quantile)

<!-- end list -->

``` r
centiles <- predict_centiles(
  newdata,
  model, 
  cent = c(25, 50, 75)
)
centiles
#> # A tibble: 4 x 4
#>     age   q25   q50   q75
#>   <dbl> <dbl> <dbl> <dbl>
#> 1     8  20.3  22.0  23.7
#> 2    10  21.7  23.4  25.1
#> 3    12  23.0  24.7  26.4
#> 4    14  24.3  26.0  27.7
```

Those predicted centiles are in wide format. We can tidy them into a
long format with `pivot_centiles_longer()`. This also includes `.pair`
column that helps mark commonly paired quantiles 25:75, 10:90, and 5:95.

``` r
pivot_centiles_longer(centiles)
#> # A tibble: 12 x 4
#>      age .quantile .value .pair           
#>    <dbl> <chr>      <dbl> <chr>           
#>  1     8 25          20.3 quantiles 25, 75
#>  2     8 50          22.0 median          
#>  3     8 75          23.7 quantiles 25, 75
#>  4    10 25          21.7 quantiles 25, 75
#>  5    10 50          23.4 median          
#>  6    10 75          25.1 quantiles 25, 75
#>  7    12 25          23.0 quantiles 25, 75
#>  8    12 50          24.7 median          
#>  9    12 75          26.4 quantiles 25, 75
#> 10    14 25          24.3 quantiles 25, 75
#> 11    14 50          26.0 median          
#> 12    14 75          27.7 quantiles 25, 75
```
