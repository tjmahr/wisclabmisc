# Compute overlap rate for (phoneme alignment) intervals

Compute overlap rate for (phoneme alignment) intervals

## Usage

``` r
compute_overlap_rate(x1, x2, y1, y2)
```

## Arguments

- x1, x2:

  start and end times for the first interval

- y1, y2:

  start and end times for the second interval

## Value

the overlap rate

## Details

Paulo and Oliveira (2004) provide an "overlap rate" statistic for
computing the amount of overlap between two (time) intervals. To my
knowledge, nobody has described the Overlap Rate in this way, but it is
the [Jaccard index](https://en.wikipedia.org/wiki/Jaccard_index) applied
to time intervals.

Let \\X=\[x\_\text{min}, x\_\text{max}\]\\ and \\Y=\[y\_\text{min},
y\_\text{max}\]\\ be the sets of times spanned by the intervals \\x\\
and \\y\\. Then, \\X \cap Y\\ is the *intersection* or the times covered
by both intervals, and \\X \cup Y\\ is the *union* or the times covered
by either interval. The size of a set \\A\\ is denoted \\\|A\|\\. Then
the overlap rate is the Jaccard index or the proportion of elements that
the two sets have in common:

\$\$\text{overlap rate} = \frac{\|X \cap Y\|}{\|X \cup Y\|}\$\$

## References

Paulo, S., & Oliveira, L. C. (2004). Automatic Phonetic Alignment and
Its Confidence Measures. In J. L. Vicedo, P. Martínez-Barco, R. Muńoz, &
M. Saiz Noeda (Eds.), *Advances in Natural Language Processing* (pp.
36–44). Springer. <https://doi.org/10.1007/978-3-540-30228-5_4>

## Examples

``` r
compute_overlap_rate(
  c(0.0, 0.0, 0.0, 0.0),
  c(1.0, 1.0, 1.0,  NA),
  c(0.5, 2.0, 1.0, 1.0),
  c(2.0, 3.0, 2.0, 2.0)
)
#> [1] 0.25 0.00 0.00   NA
```
