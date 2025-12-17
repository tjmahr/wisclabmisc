# Convert age in months to years;months

Convert age in months to years;months

## Usage

``` r
format_year_month_age(x)
```

## Arguments

- x:

  a vector ages in months

## Value

ages in the years;months format

## Details

Ages of `NA` return `"NA;NA"`.

This format by default is not numerically ordered. This means that
`c("2;0", "10;10", "10;9")` would sort as `c("10;10", "10;9", "2;0")`.
The function `stringr::str_sort(..., numeric = TRUE)` will sort this
vector correctly.

## Examples

``` r
ages <- c(26, 58, 25, 67, 21, 59, 36, 43, 27, 49)
format_year_month_age(ages)
#>  [1] "2;2"  "4;10" "2;1"  "5;7"  "1;9"  "4;11" "3;0"  "3;7"  "2;3"  "4;1" 
```
