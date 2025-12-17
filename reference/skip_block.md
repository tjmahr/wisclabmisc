# Skip a block of code without executing it

`skip_block()` is a lightweight control-flow helper to deliberately skip
execution of a block of code while still documenting that the block
exists. It is intended as an alternative to commenting out code or
wrapping code in `if (FALSE) { ... }`.

## Usage

``` r
skip_block(...)
```

## Arguments

- ...:

  Either a single braced expression containing the code block to skip,
  or a character string followed by a single braced expression. When a
  message is supplied, it is printed instead of the default message.

## Value

`NULL`, invisibly.

## Details

The function captures the unevaluated code block, reports how many lines
would have run, optionally prints a user-supplied message, and then
returns invisibly without evaluating the code.

`skip_block()` uses non-standard evaluation to capture the code block
via [`substitute()`](https://rdrr.io/r/base/substitute.html). The code
block is never evaluated.

Supplying more than two arguments, or supplying no code block, is an
error.

## Examples

``` r
skip_block({
  Sys.sleep(10)
})
#> Skipping code block (3 lines)

skip_block("Skipping slow preprocessing step", {
    Sys.sleep(10)
  }
)
#> Skipping slow preprocessing step (3 lines)
```
