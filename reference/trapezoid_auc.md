# Compute AUCs using the trapezoid method

Compute AUCs using the trapezoid method

## Usage

``` r
trapezoid_auc(xs, ys)

partial_trapezoid_auc(xs, ys, xlim)
```

## Arguments

- xs, ys:

  x and y positions

- xlim:

  two-element vector (a range) of the `xs` to sum over

## Value

the area under the curve computed using the trapezoid method. For
`partial_trapezoid_auc()`, the partial area under the curve is computed.

## Examples

``` r
# Predict whether a car has automatic vs. manual transmission from mpg
r <- pROC::roc(am ~ mpg, mtcars)
#> Setting levels: control = 0, case = 1
#> Setting direction: controls < cases
pROC::auc(r)
#> Area under the curve: 0.83
trapezoid_auc(r$specificities, r$sensitivities)
#> [1] 0.8299595

pROC::auc(r, partial.auc = c(.9, 1), partial.auc.focus = "sp")
#> Partial area under the curve (specificity 1-0.9): 0.04779
partial_trapezoid_auc(r$specificities, r$sensitivities, c(.9, 1))
#> [1] 0.04779352

pROC::auc(r, partial.auc = c(.9, 1), partial.auc.focus = "se")
#> Partial area under the curve (sensitivity 1-0.9): 0.02996
partial_trapezoid_auc(r$sensitivities, r$specificities, c(.9, 1))
#> [1] 0.02408907

pROC::auc(r, partial.auc = c(.1, .9), partial.auc.focus = "sp")
#> Partial area under the curve (specificity 0.9-0.1): 0.6822
partial_trapezoid_auc(r$specificities, r$sensitivities, c(.1, .9))
#> [1] 0.682166

pROC::auc(r, partial.auc = c(.1, .9), partial.auc.focus = "se")
#> Partial area under the curve (sensitivity 0.9-0.1): 0.7
partial_trapezoid_auc(r$sensitivities, r$specificities, c(.1, .9))
#> [1] 0.695749
```
