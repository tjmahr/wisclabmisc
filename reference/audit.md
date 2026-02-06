# A lightweight container for auditing or logging

An `audit` is a [`list()`](https://rdrr.io/r/base/list.html) with two
fields:

1.  `data`: most recently updated (or poked) data

2.  `notes`: a history of the `fn` calls and results applied to the
    object.

Usage:

- `audit_wrap(data)` wraps some data inside of an audit.

- `audit_peek(x, fn, ...)` applies a function to the data, log the
  results in the `notes`, but *not* overwrite the audit data.

- `audit_poke(x, fn, ...)` applies a function to the data, log the
  results in the `notes`, and overwrite the audit data.

- `audit_unwrap()` returns the underlying data.

## Usage

``` r
audit_wrap(data)

audit_peek(x, fn, ...)

audit_poke(x, fn, ...)

audit_unwrap(x)
```

## Arguments

- data:

  data to wrap into an audit

- x:

  an audit object

- fn:

  a function to apply to the data inside of the audit object

- ...:

  further arguments for `fn`

## Value

`audit_wrap()`, `audit_peek()`, `audit_poke()` return audit objects.
`audit_unwrap()` returns the `$data` from an `audit`.

## Details

There is no `audit_notes()` function at this time because it is not
clear what the most convenient or tidy format would be for the notes.

These container was designed for piping a vector of filenames through a
series of data validation functions. Therefore, the intended use case is
a vector of a data and not larger objects.

## Examples

``` r
x <- letters |>
  audit_wrap() |>
  audit_peek(rev) |>
  audit_peek(toupper)

# Note that none of the underlying data has changed. This is useful for
# finding values that fail validation checks.
x
#> <audit> object
#> data: chr [1:26] "a" "b" "c" "d" ...
#> notes:
#> > rev(x) [peek] : chr [1:26] "z" "y" "x" "w" ...
#> > toupper(x) [peek]: chr [1:26] "A" "B" "C" "D" ...

# Use _poke() to update the data:
x <- letters |>
  audit_wrap() |>
  audit_peek(rev) |>
  audit_poke(toupper) |>
  audit_peek(getElement, 1) |>
  audit_peek(function(x) character(0))
x
#> <audit> object
#> data: chr [1:26] "A" "B" "C" "D" ...
#> notes:
#> > rev(x) [peek] : chr [1:26] "z" "y" "x" "w" ...
#> > toupper(x) [poke] : chr [1:26] "A" "B" "C" "D" ...
#> > getElement(x, 1) [peek] : chr "A"
#> > `<unnamed-function-4>`(x) [peek]: chr(0)

# Index into the notes to pull the data
x$notes[[3]][["result"]]
#> [1] "A"
```
