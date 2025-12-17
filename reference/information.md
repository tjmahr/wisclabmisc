# Compute entropy and related measures

Compute entropy and related measures

## Usage

``` r
info_surprisal(x, base = NULL)

info_entropy(p, base = NULL)

info_cross_entropy(p, q, base = NULL)

info_kl_divergence(p, q, base = NULL)

info_kl_divergence_matrix(mat, base = NULL)
```

## Arguments

- x:

  a vector of probabilities

- base:

  the base of the logarithm. Defaults to *e* (`exp(1)`).

- p, q:

  a probability distribution (vector of probabilities that sum to 1)

- mat:

  a matrix where each row is a probability distribution

## Value

the entropy

## Information theory basics

Given a probability `x`, the **surprisal value** (or information
content) of `x` is the log of the inverse probability, `log(1 / x)`.
Rare events (smaller probabilities) have larger surprisal values than
more common events. The word "surprise" here conveys how unexpected or
informative an event is. (The idea that surprises are "informative" or
contain information makes sense with the intuition that there isn't much
to be learned from predictable events.) Because of how division works
with logarithms, `log(1 / x)` simplifies so that `info_surprisal(x)` is
the negative log probability,`-log(x)`. The units of the information
content depends on the base of the logarithm. Our functions by default
use the natural [`log()`](https://rdrr.io/r/base/Log.html) which
provides information in *nats*, but it is also common to see
[`log2()`](https://rdrr.io/r/base/Log.html)-based surprisals which
provide information in *bits*.

Given a probability distribution `p`—a vector of probabilities that sum
to 1—the **entropy** of the distribution is the probability-weighted
average of the surprisal values. A weighted average is
`sum(weights * values) / sum(weights)`, but because the weights here are
probabilities that sum to 1, the `info_entropy(p)` is
`sum(p * info_surprisal(p))`. Entropy can be interpreted as a measure of
uncertainty in the distribution; it's the expected surprisal value in
the distribution.

In `info_entropy(p)` the surprisal values and their weights came from
the same probability distribution. But this doesn't need to be the case.
Suppose that there are ground-truth probabilities `p` and there are also
estimated probabilities `q`. `info_entropy(q)` computes a weighted
average where events with surprisal `info_surprisal(q)` have frequencies
of `q`. But if we knew the true frequencies, the surprisals in `q` would
occur with frequency `p`, so their weighted average should be
`sum(p * info_surprisal(q))`. This value is the **cross entropy** of `q`
with respect to `p`, and `info_cross_entropy(p, q)` implements this
function. Cross entropy is commonly used as a loss function in machine
learning.

[Gibb's inequality](https://en.wikipedia.org/wiki/Gibbs%27_inequality)
says that `info_entropy(p) <= info_cross_entropy(p, q)`. Unless `p` and
`q` are the same distribution, there will always be some excess
uncertainty or surprise in the cross entropy compared to the entropy:
`info_cross_entropy(p, q) = info_entropy(p) + *excess*`. This excess is
the **Kullback-Liebler divergence** (KL divergence or relative entropy).
Due to the properties of logarithms, `info_kl_divergence(p, q)`
simplifies to `sum (p * log(p / q))`. KL divergence is an important
quantity for comparing probability distributions. For example, each row
in a confusion matrix is a probability distribution, and KL divergence
can identify which rows are most similar.
`info_kl_divergence_matrix(mat)` computes a distance matrix using on
each pair of rows in `mat` using KL divergence.

## Examples

``` r
wikipedia_example <- rbind(
  p = c(9, 12, 4) / 25,
  q = c(1,  1, 1) / 3
)

info_kl_divergence_matrix(wikipedia_example)
#>            p         q
#> p 0.00000000 0.0852996
#> q 0.09745501 0.0000000
```
