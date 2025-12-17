# Create an ROC curve from observed data

Create an ROC curve from observed data

## Usage

``` r
compute_empirical_roc(
  data,
  response,
  predictor,
  direction = "auto",
  best_weights = c(1, 0.5),
  levels = NULL,
  ...
)
```

## Arguments

- data:

  a dataframe containing responses (groupings) and predictor variable

- response:

  a bare column name with the group status (control vs. cases). If the
  response has more than two groups, the first element of `levels` is
  the control group and the second element of `levels` is the case
  group.

- predictor:

  a bare column name with the predictor to use for classification

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

- best_weights:

  weights for computing the best ROC curve points. Defaults to
  `c(1, .5)`, which are the defaults used by
  [`pROC::coords()`](https://rdrr.io/pkg/pROC/man/coords.html).

- levels:

  two-element vector `c(control, case)` where `control` is the value of
  `response` for the control group and `case` is the value of `response`
  for the case group. The ordering matters: The first element of the
  vector names the control group.

- ...:

  additional arguments passed to
  [`pROC::roc()`](https://rdrr.io/pkg/pROC/man/roc.html).

## Value

a new dataframe of ROC coordinates is returned with columns for the
predictor variable, `.sensitivities`, `.specificities`, `.auc`,
`.direction`, `.controls`, `.cases`, `.n_controls`, `.n_cases`,
`.is_best_youden` and `.is_best_closest_topleft`.

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

# get an ROC on the fake data
compute_empirical_roc(data, outcome, y)
#> ℹ No `levels` provided. Using `levels = c("0", "1")`.
#> • Setting control to outcome == "0".
#> • Setting case to outcome == "1".
#> Setting direction: controls > cases
# this guess the cases and controls from the group name and gets it wrong
compute_empirical_roc(data, group, y)
#> ℹ No `levels` provided. Using `levels = c("case", "control")`.
#> • Setting control to group == "case".
#> • Setting case to group == "control".
#> Setting direction: controls < cases
# better
compute_empirical_roc(data, group, y, levels = c("control", "case"))
#> Setting direction: controls > cases
```
