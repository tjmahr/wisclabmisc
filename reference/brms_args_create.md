# Set default arguments for brms model fitting

Set default arguments for brms model fitting

## Usage

``` r
brms_args_create(
  ...,
  .backend = "cmdstanr",
  .threads = 2,
  .chains = 4,
  .cores = 4,
  .iter = 2000,
  .silent = 0,
  .file_refit = "on_change",
  .refresh = 25,
  .control = list()
)
```

## Arguments

- ...:

  arguments to
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html) to
  use as default values.

- .backend, .threads, .chains, .cores, .iter, .silent, .file_refit,
  .refresh, .control:

  These arguments set *default* default values. Overwrite these defaults
  by using the argument without the `.` prefix.

## Value

A function for generating a list of arguments to
[`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html).

## Details

One can set the default value for `adapt_delta` directly
(`adapt_delta = .98`). This value will be propagated to
`control$adapt_delta`.

## Examples

``` r
brm_args <- brms_args_create()

# using package-provided defaults
brm_args()
#> List of 9
#>  $ backend   : chr "cmdstanr"
#>  $ threads   : num 2
#>  $ chains    : num 4
#>  $ cores     : num 4
#>  $ iter      : num 2000
#>  $ silent    : num 0
#>  $ file_refit: chr "on_change"
#>  $ refresh   : num 25
#>  $ control   : list()
#>  - attr(*, "class")= chr [1:2] "brm_args" "list"

# overwriting a default value
brm_args(iter = 500)
#> List of 9
#>  $ backend   : chr "cmdstanr"
#>  $ threads   : num 2
#>  $ chains    : num 4
#>  $ cores     : num 4
#>  $ iter      : num 500
#>  $ silent    : num 0
#>  $ file_refit: chr "on_change"
#>  $ refresh   : num 25
#>  $ control   : list()
#>  - attr(*, "class")= chr [1:2] "brm_args" "list"

# adapt_delta is handled specially
brm_args(adapt_delta = .95)
#> List of 9
#>  $ backend   : chr "cmdstanr"
#>  $ threads   : num 2
#>  $ chains    : num 4
#>  $ cores     : num 4
#>  $ iter      : num 2000
#>  $ silent    : num 0
#>  $ file_refit: chr "on_change"
#>  $ refresh   : num 25
#>  $ control   :List of 1
#>   ..$ adapt_delta: num 0.95
#>  - attr(*, "class")= chr [1:2] "brm_args" "list"
# adapt_delta is handled specially
brm_args(adapt_delta = .95)
#> List of 9
#>  $ backend   : chr "cmdstanr"
#>  $ threads   : num 2
#>  $ chains    : num 4
#>  $ cores     : num 4
#>  $ iter      : num 2000
#>  $ silent    : num 0
#>  $ file_refit: chr "on_change"
#>  $ refresh   : num 25
#>  $ control   :List of 1
#>   ..$ adapt_delta: num 0.95
#>  - attr(*, "class")= chr [1:2] "brm_args" "list"

# We can overwrite the package-provided defaults
other_brm_args <- brms_args_create(iter = 4000, backend = "rstan")
other_brm_args()
#> List of 9
#>  $ backend   : chr "rstan"
#>  $ threads   : num 2
#>  $ chains    : num 4
#>  $ cores     : num 4
#>  $ iter      : num 4000
#>  $ silent    : num 0
#>  $ file_refit: chr "on_change"
#>  $ refresh   : num 25
#>  $ control   : list()
#>  - attr(*, "class")= chr [1:2] "brm_args" "list"

# And overwrite them too
other_brm_args(backend = "cmdstanr")
#> List of 9
#>  $ backend   : chr "cmdstanr"
#>  $ threads   : num 2
#>  $ chains    : num 4
#>  $ cores     : num 4
#>  $ iter      : num 4000
#>  $ silent    : num 0
#>  $ file_refit: chr "on_change"
#>  $ refresh   : num 25
#>  $ control   : list()
#>  - attr(*, "class")= chr [1:2] "brm_args" "list"
```
