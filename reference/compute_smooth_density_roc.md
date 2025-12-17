# Create an ROC curve from smoothed densities

Create an ROC curve from smoothed densities

## Usage

``` r
compute_smooth_density_roc(
  data,
  controls,
  cases,
  along = NULL,
  best_weights = c(1, 0.5),
  direction = "auto"
)
```

## Arguments

- data:

  a dataframe containing densities

- controls, cases:

  bare column names for the densities of the control group and case
  group

- along:

  optional bare column name for the response values

- best_weights:

  weights for computing the best ROC curve points. Defaults to
  `c(1, .5)`, which are the defaults used by
  [`pROC::coords()`](https://rdrr.io/pkg/pROC/man/coords.html).

- direction:

  `direction` for computing case status.
  [`pROC::roc()`](https://rdrr.io/pkg/pROC/man/roc.html)'s `direction`
  conventions are supported: `"<"` (i.e., `control < case`) and `">"`
  (`control > case`), or `"auto"` to have pROC guess the direction (if
  applicable). Alternatively, more verbose directions are supported:
  `"case-low"` or `"control-high"` if a low score predicts case status
  (`case <= threshold < control`, analogous to pROC's `">"`) and
  `"case-high"` or `"control-low"` if a high score predicts case status
  (`control < threshold <= case`, analogous to pROC's `"<"`). These
  directions are translated into the pROC conventions.

## Value

the dataframe is updated with new columns for the `.sensitivities`,
`.specificities`, `.auc`, `.roc_row`, `.is_best_youden` and
`.is_best_closest_topleft`.

## Examples

``` r
set.seed(100)
x1 <- rnorm(100, 4, 1)
x2 <- rnorm(100, 2, .5)
both <- c(x1, x2)
steps <- seq(min(both), max(both), length.out = 200)
d1 <- dnorm(steps, mean(x1), sd(x1))
d2 <- dnorm(steps, mean(x2), sd(x2))
data <- tibble::tibble(
  y = steps,
  d1 = d1,
  d2 = d2,
  outcome = rbinom(200, 1, prob = 1 - (d1 / (d1 + d2))),
  group = ifelse(outcome, "case", "control")
)
compute_smooth_density_roc(data, d1, d2)
#> # A tibble: 202 × 12
#>        y      d1     d2 outcome group .sensitivities .specificities  .auc
#>    <dbl>   <dbl>  <dbl>   <int> <chr>          <dbl>          <dbl> <dbl>
#>  1 0.932 0.00423 0.0264       1 case        0.000751          1.000 0.967
#>  2 0.960 0.00460 0.0319       1 case        0.00166           1.000 0.967
#>  3 0.989 0.00499 0.0383       1 case        0.00275           1.000 0.967
#>  4 1.02  0.00542 0.0459       1 case        0.00406           0.999 0.967
#>  5 1.05  0.00587 0.0546       1 case        0.00561           0.999 0.967
#>  6 1.07  0.00636 0.0647       1 case        0.00746           0.999 0.967
#>  7 1.10  0.00689 0.0763       1 case        0.00963           0.999 0.967
#>  8 1.13  0.00745 0.0895       1 case        0.0122            0.999 0.967
#>  9 1.16  0.00806 0.104        1 case        0.0152            0.998 0.967
#> 10 1.19  0.00870 0.121        1 case        0.0186            0.998 0.967
#> # ℹ 192 more rows
#> # ℹ 4 more variables: .roc_row <int>, .direction <chr>, .is_best_youden <lgl>,
#> #   .is_best_closest_topleft <lgl>
compute_smooth_density_roc(data, d1, d2, along = y)
#> # A tibble: 202 × 12
#>        y      d1     d2 outcome group .sensitivities .specificities  .auc
#>    <dbl>   <dbl>  <dbl>   <int> <chr>          <dbl>          <dbl> <dbl>
#>  1 0.932 0.00423 0.0264       1 case        0.000751          1.000 0.967
#>  2 0.960 0.00460 0.0319       1 case        0.00166           1.000 0.967
#>  3 0.989 0.00499 0.0383       1 case        0.00275           1.000 0.967
#>  4 1.02  0.00542 0.0459       1 case        0.00406           0.999 0.967
#>  5 1.05  0.00587 0.0546       1 case        0.00561           0.999 0.967
#>  6 1.07  0.00636 0.0647       1 case        0.00746           0.999 0.967
#>  7 1.10  0.00689 0.0763       1 case        0.00963           0.999 0.967
#>  8 1.13  0.00745 0.0895       1 case        0.0122            0.999 0.967
#>  9 1.16  0.00806 0.104        1 case        0.0152            0.998 0.967
#> 10 1.19  0.00870 0.121        1 case        0.0186            0.998 0.967
#> # ℹ 192 more rows
#> # ℹ 4 more variables: .roc_row <int>, .direction <chr>, .is_best_youden <lgl>,
#> #   .is_best_closest_topleft <lgl>

# terrible ROC because the response is not present (just the densities)
data_shuffled <- data[sample(seq_len(nrow(data))), ]
compute_smooth_density_roc(data_shuffled, d1, d2)
#> # A tibble: 202 × 12
#>        y     d1       d2 outcome group   .sensitivities .specificities  .auc
#>    <dbl>  <dbl>    <dbl>   <int> <chr>            <dbl>          <dbl> <dbl>
#>  1  2.29 0.0963 7.70e- 1       1 case            0.0219          0.997 0.551
#>  2  3.43 0.334  1.66e- 3       0 control         0.0220          0.988 0.551
#>  3  4.40 0.363  1.49e- 8       0 control         0.0220          0.977 0.551
#>  4  6.33 0.0293 2.63e-26       0 control         0.0220          0.976 0.551
#>  5  6.35 0.0275 1.21e-26       0 control         0.0220          0.976 0.551
#>  6  5.59 0.117  2.59e-18       0 control         0.0220          0.972 0.551
#>  7  3.00 0.242  4.30e- 2       0 control         0.0232          0.965 0.551
#>  8  3.26 0.300  7.00e- 3       1 case            0.0234          0.957 0.551
#>  9  3.20 0.288  1.09e- 2       0 control         0.0237          0.949 0.551
#> 10  4.20 0.384  2.64e- 7       0 control         0.0237          0.938 0.551
#> # ℹ 192 more rows
#> # ℹ 4 more variables: .roc_row <int>, .direction <chr>, .is_best_youden <lgl>,
#> #   .is_best_closest_topleft <lgl>

# sorted along response first: correct AUC
compute_smooth_density_roc(data_shuffled, d1, d2, along = y)
#> # A tibble: 202 × 12
#>        y     d1       d2 outcome group   .sensitivities .specificities  .auc
#>    <dbl>  <dbl>    <dbl>   <int> <chr>            <dbl>          <dbl> <dbl>
#>  1  2.29 0.0963 7.70e- 1       1 case             0.776        0.952   0.967
#>  2  3.43 0.334  1.66e- 3       0 control          1.000        0.707   0.967
#>  3  4.40 0.363  1.49e- 8       0 control          1.000        0.342   0.967
#>  4  6.33 0.0293 2.63e-26       0 control          1            0.00551 0.967
#>  5  6.35 0.0275 1.21e-26       0 control          1            0.00472 0.967
#>  6  5.59 0.117  2.59e-18       0 control          1            0.0534  0.967
#>  7  3.00 0.242  4.30e- 2       0 control          0.995        0.833   0.967
#>  8  3.26 0.300  7.00e- 3       1 case             0.999        0.762   0.967
#>  9  3.20 0.288  1.09e- 2       0 control          0.999        0.779   0.967
#> 10  4.20 0.384  2.64e- 7       0 control          1.000        0.416   0.967
#> # ℹ 192 more rows
#> # ℹ 4 more variables: .roc_row <int>, .direction <chr>, .is_best_youden <lgl>,
#> #   .is_best_closest_topleft <lgl>
```
