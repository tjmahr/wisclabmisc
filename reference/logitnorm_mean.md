# Compute the mean of logit-normal distribution(s)

This function is a wrapper around
[`logitnorm::momentsLogitnorm()`](https://rdrr.io/pkg/logitnorm/man/momentsLogitnorm.html).

## Usage

``` r
logitnorm_mean(mu, sigma)
```

## Arguments

- mu:

  mean(s) on the logit scale

- sigma:

  standard deviation(s) on the logit scale

## Value

the means of the distributions

## Examples

``` r
# \donttest{
x <- logitnorm_mean(2, 1)
x
#>      mean 
#> 0.8445375 
# }
# compare to simulation
set.seed(100)
rnorm(1000, 2, 1) |> plogis() |> mean()
#> [1] 0.8444758
```
