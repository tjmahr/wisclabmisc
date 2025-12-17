# Compute chronological age in months

Ages are rounded down to the nearest month. A difference of 20 months,
29 days is interpreted as 20 months.

## Usage

``` r
chrono_age(t1, t2)
```

## Arguments

- t1, t2:

  dates in "yyyy-mm-dd" format

## Value

the chronological ages in months. NA is returned if the age cannot be
computed.

## Examples

``` r
# Two years exactly
chrono_age("2014-01-20", "2012-01-20")
#> [1] 24
#> 24

# Shift a year
chrono_age("2014-01-20", "2013-01-20")
#> [1] 12
#> 12
chrono_age("2014-01-20", "2011-01-20")
#> [1] 36
#> 36

# Shift a month
chrono_age("2014-01-20", "2012-02-20")
#> [1] 23
#> 23
chrono_age("2014-01-20", "2011-12-20")
#> [1] 25
#> 25

# 3 months exactly
chrono_age("2014-05-10", "2014-02-10")
#> [1] 3
#> 3

# Borrow a month when the earlier date has a later day
chrono_age("2014-05-10", "2014-02-11")
#> [1] 2
#> 2, equal to 2 months, 29 days rounded down to nearest month

# Inverted argument order
chrono_age("2012-01-20", "2014-01-20")
#> [1] 24
#> 24

# Multiple dates
t1 <- c("2012-01-20", "2014-02-10", "2010-10-10")
t2 <- c("2014-01-20", "2014-05-10", "2014-11-10")
chrono_age(t1, t2)
#> [1] 24  3 49
#> [1] 24  3 49
```
