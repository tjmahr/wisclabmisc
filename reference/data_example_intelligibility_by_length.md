# Simulated intelligibility scores by utterance length

A dataset of simulated intelligibility scores for testing and
demonstrating modeling functions. These were created by fitting a
Bayesian model of the raw Hustad and colleagues (2020) and drawing 1
sample from the posterior distribution of expected predictions (i.e.,
"epreds). In other words, these values are model predictions of the
original dataset. They are correlated with original dataset values at
*r* = .86. We might think of the simulation as adding random noise to
the original dataset.

## Usage

``` r
data_example_intelligibility_by_length
```

## Format

A data frame with 694 rows and 5 variables:

- child:

  identifier for the child

- age_months:

  child's age in months

- length_longest:

  length of the child's longest utterance

- tocs_level:

  utterance length

- sim_intelligibility:

  child's intelligibility for the given utterance length (proportion of
  words said by the child that were correctly transcribed by two
  listeners)

## References

Hustad, K. C., Mahr, T., Natzke, P. E. M., & Rathouz, P. J. (2020).
Development of Speech Intelligibility Between 30 and 47 Months in
Typically Developing Children: A Cross-Sectional Study of Growth.
*Journal of Speech, Language, and Hearing Research*, *63*(6), 1675–1687.
https://doi.org/10.1044/2020_JSLHR-20-00008
