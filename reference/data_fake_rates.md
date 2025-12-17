# Fake speaking rate data

A dataset of fake speaking rate measures for testing and demonstrating
modeling functions. These were created by randomly sampling 200 rows of
a speaking rate dataset and adding random noise to the `age_months` and
`speaking_sps` variables. These values do not measure any real children
but represent plausible age and rate measurements from our kind of work.

## Usage

``` r
data_fake_rates
```

## Format

A data frame with 200 rows and 2 variables:

- age_months:

  child's age in months

- speaking_sps:

  child's speaking rate in syllables per second
