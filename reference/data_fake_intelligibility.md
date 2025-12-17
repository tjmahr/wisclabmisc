# Fake intelligibility data

A dataset of fake intelligibility scores for testing and demonstrating
modeling functions. These were created by randomly sampling 200 rows of
an intelligibility dataset and adding random noise to the `age_months`
and `intelligibility` variables. These values do not measure any real
children but represent plausible age and intelligibility measurements
from our kind of work.

## Usage

``` r
data_fake_intelligibility
```

## Format

A data frame with 200 rows and 2 variables:

- age_months:

  child's age in months

- intelligibility:

  child's intelligibility (proportion of words said by the child that
  were correctly transcribed by two listeners)
