# Convert between age in months, years;months, and yymm age formats

Convert between age in months, years;months, and yymm age formats

## Usage

``` r
format_year_month_age(x, sep = ";")

parse_year_month_age(x, sep = ";")

parse_yymm_age(x, start = 1L)
```

## Arguments

- x:

  - `format_year_month_age()`: a numeric vector of (non-negative) ages
    in months.

  - `parse_year_month_age()`: a character vector of ages in
    `"years;months"` format (or `years{sep}months` format more
    generally).

  - `parse_yymm_age()`: a character vector of ages in `"yymm"` format.

- sep:

  Separator to use for `year_month` functions. Defaults to `;`.

- start:

  For `parse_yymm_age()`, the location of the starting character the
  `yymm` sequence. Defaults to 1.

## Value

- `format_year_month_age()` returns a character vector in
  `"years;months"` format (or `years{sep}months` format more generally).

- `parse_year_month_age()` returns a vector of ages in months.

- `parse_yymm_age()`: returns a vector of ages in months.

## Details

For `format_year_month_age()`, ages of `NA` return `"NA;NA"`.

For `parse_year_month_age()`, values that cannot be parsed return `NA`.

This format by default is not numerically ordered. This means that
`c("2;0", "10;10", "10;9")` would sort as `c("10;10", "10;9", "2;0")`.
The function `stringr::str_sort(..., numeric = TRUE)` will sort this
vector correctly.

## Examples

``` r
ages <- c(26, 58, 25, 67, 21, 59, 36, 43, 27, 49, NA)
ym_ages <- format_year_month_age(ages)
ym_ages
#>  [1] "2;2"   "4;10"  "2;1"   "5;7"   "1;9"   "4;11"  "3;0"   "3;7"   "2;3"  
#> [10] "4;1"   "NA;NA"

parse_year_month_age(ym_ages)
#>  [1] 26 58 25 67 21 59 36 43 27 49 NA

parse_yymm_age(c("0204", "0310"))
#> [1] 28 46

parse_yymm_age(c("ab_0204", "ab_0310"), start = 4)
#> [1] 28 46
```
