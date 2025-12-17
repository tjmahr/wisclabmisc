# Fit a gamlss model but store user data

Think of it as a gamlss model with memories (mem. gamlss).

## Usage

``` r
mem_gamlss(...)
```

## Arguments

- ...:

  arguments passed to gamlss::gamlss()

## Value

the fitted `model` object but updated to include user information in
`model$.user`. Includes the dataset used to fit the model
`model$.user$data`, the session info `model$.user$session_info` and the
call used to fit the model `model$.user$call`. `model$call` is updated
to match
