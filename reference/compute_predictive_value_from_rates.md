# Compute positive and negative predictive value

Compute positive and negative predictive value

## Usage

``` r
compute_predictive_value_from_rates(sensitivity, specificity, prevalence)
```

## Arguments

- sensitivity, specificity, prevalence:

  vectors of confusion matrix rates

## Value

a tibble with the columns `sensitivity`, `specificity`, `prevalence`,
`ppv`, `npv` where `ppv` and `npv` are the positive predictive value and
the negative predictive value.

## Details

These vectors passed into this function should be some common length
and/or length 1. For example, 4 sensitivities, 4 specificities and 1
incidence will work because the sensitivities and specificities have a
common length and we can safely recycle (reuse) the incidence value. But
4 sensitivities, 2 specificities, and 1 incidence will *fail* because
there is not a common length.

## Examples

``` r
compute_predictive_value_from_rates(
  sensitivity = .9,
  specificity = .8,
  prevalence = .05
)
#> # A tibble: 1 × 5
#>   sensitivity specificity prevalence   ppv   npv
#>         <dbl>       <dbl>      <dbl> <dbl> <dbl>
#> 1         0.9         0.8       0.05 0.191 0.993

compute_predictive_value_from_rates(
  sensitivity = .67,
  specificity = .53,
  prevalence = c(.15, .3)
)
#> # A tibble: 2 × 5
#>   sensitivity specificity prevalence   ppv   npv
#>         <dbl>       <dbl>      <dbl> <dbl> <dbl>
#> 1        0.67        0.53       0.15 0.201 0.901
#> 2        0.67        0.53       0.3  0.379 0.789
```
