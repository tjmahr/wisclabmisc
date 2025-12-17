# Extract the TOCS details from a string (usually a filename)

Extract the TOCS details from a string (usually a filename)

## Usage

``` r
tocs_item(xs)

tocs_type(xs)

tocs_length(xs)
```

## Arguments

- xs:

  a character vector

## Value

`tocs_item()` returns the substring with the TOCS item, `tocs_type()`
returns whether the item is `"single-word"` or `"multiword"`, and
`tocs_length()` returns the length of the TOCS item (i.e., the number of
words).

## Examples

``` r
x <- c(
  "XXv16s7T06.lab", "XXv15s5T06.TextGrid", "XXv13s3T10.WAV",
  "XXv18wT11.wav", "non-matching", "s2T01",
  "XXv01s4B01.wav", "XXv01wB01.wav",
  # sometimes these have tags for *v*irtual visits or recording attempts
  "XXv13s3T10v.WAV", "XXv13s3T10a.lab"
)
data.frame(
  x = x,
  item = tocs_item(x),
  type = tocs_type(x),
  length = tocs_length(x)
)
#>                      x  item        type length
#> 1       XXv16s7T06.lab S7T06   multiword      7
#> 2  XXv15s5T06.TextGrid S5T06   multiword      5
#> 3       XXv13s3T10.WAV S3T10   multiword      3
#> 4        XXv18wT11.wav  WT11 single-word      1
#> 5         non-matching  <NA>        <NA>     NA
#> 6                s2T01 S2T01   multiword      2
#> 7       XXv01s4B01.wav S4B01   multiword      4
#> 8        XXv01wB01.wav  WB01 single-word      1
#> 9      XXv13s3T10v.WAV S3T10   multiword      3
#> 10     XXv13s3T10a.lab S3T10   multiword      3
```
