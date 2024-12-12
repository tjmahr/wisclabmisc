

#' Uniroot function that doesn't throw errors
#'
#' This function is a wrapper around [stats::uniroot()] that returns a list of
#' `NA` values when [stats::uniroot()] would raise an error. This function is
#' useful, for example, when checking roots on many many curves (a bootstrap
#' sample or posterior sample), and there is no guarantee that every curve has a
#' root.
#'
#' @param ... arguments
#' @return for safe inputs, the results of `stats::uniroot(...)`. For failing
#'   inputs, a list with the elements `root`, `f.root`, `iter`, `init.it`,
#'   `estim.prec`, and each has the value `NA`.
#' @noRd
try_uniroot <- function(...) {
  tryCatch(
    stats::uniroot(...),
    error = function(e) {
      list(
        root = NA_real_,
        f.root = NA_real_,
        iter = NA_integer_,
        init.it = NA_integer_,
        estim.prec = NA_real_
      )
    }
  )
}


#' Compute the mean of logit-normal distribution(s)
#'
#' This function is a wrapper around [logitnorm::momentsLogitnorm()].
#'
#' @param mu mean(s) on the logit scale
#' @param sigma standard deviation(s) on the logit scale
#' @return the means of the distributions
#' @export
#' @examples
#' \donttest{
#' x <- logitnorm_mean(2, 1)
#' x
#' }
#' # compare to simulation
#' set.seed(100)
#' rnorm(1000, 2, 1) |> plogis() |> mean()
logitnorm_mean <- function(mu, sigma) {
  rlang::check_installed("logitnorm", reason = "for `logitnorm_mean()`")
  x <- Vectorize(logitnorm::momentsLogitnorm)(mu = mu, sigma = sigma)
  x["mean", ]
}





#' Compute entropy and related measures
#'
#' @param x a vector of probabilities
#' @param p a probability distribution (vector of probabilities that sum to 1)
#' @param mat a matrix where each row is a probability distribution
#' @return the entropy
#' @rdname information
#' @export
#' @section Information theory basics:
#'
#' Given a probability `x`, the **surprisal value** (or information content) of
#' `x` is the log of the inverse probability, `log(1 / x)`. Rare events (smaller
#' probabilities) have larger surprisal values than more common events. The word
#' "surprise" here conveys how unexpected or informative an event is. (The idea
#' that surprises are "informative" or contain information makes sense with the
#' intuition that there isn't much to be learned from predictable events.)
#' Because of how division works with logarithms, `log(1 / x)` simplifies so
#' that `info_surprisal(x)` is the negative log probability,`-log(x)`. The units
#' of the information content depends on the base of the logarithm. Our
#' functions use the natural `log()` which provides information in *nats*, but
#' it is also common to see `log2()`-based surprisals which provide information
#' in *bits*.
#'
#' Given a probability distribution `p`---a vector of probabilities that
#' sum to 1---the **entropy** of the distribution is the
#' probability-weighted average of the surprisal values. A weighted average
#' is `sum(weights * values) / sum(weights)`, but because the weights here
#' are probabilities that sum to 1, the `info_entropy(p)` is `sum(p *
#' info_surprisal(p))`. Entropy can be interpreted as a measure of
#' uncertainty in the distribution; it's the expected surprisal value in
#' the distribution.
#'
#' In `info_entropy(p)` the surprisal values and their weights came from
#' the same probability distribution. But this doesn't need to be the case.
#' Suppose that there are ground-truth probabilities `p` and there are also
#' estimated probabilities `q`. `info_entropy(q)` computes a weighted
#' average where events with surprisal `info_surprisal(q)` have frequencies
#' of `q`. But if we knew the true frequencies, the surprisals in `q` would
#' occur with frequency `p`, so their weighted average should be `sum(p *
#' info_surprisal(q))`. This value is the **cross entropy** of `q` with
#' respect to `p`, and `info_cross_entropy(p, q)` implements this function.
#' Cross entropy is commonly used as a loss function in machine learning.
#'
#' [Gibb's inequality](https://en.wikipedia.org/wiki/Gibbs%27_inequality)
#' says that `info_entropy(p) <= info_cross_entropy(p, q)`. Unless `p` and
#' `q` are the same distribution, there will always be some excess
#' uncertainty or surprise in the cross entropy compared to the entropy:
#' `info_cross_entropy(p, q) = info_entropy(p) + *excess*`. This excess is
#' the **Kullback-Liebler divergence** (KL divergence or relative entropy).
#' Due to the properties of logarithms, `info_kl_divergence(p, q)`
#' simplifies to `sum (p * log(p / q))`. KL divergence is an important
#' quantity for comparing probability distributions. For example, each row
#' in a confusion matrix is a probability distribution, and KL divergence
#' can identify which rows are most similar. `info_kl_divergence_matrix(mat)`
#' computes a distance matrix using on each pair of rows in `mat` using KL
#' divergence.
#'
#' @examples
#' wikipedia_example <- rbind(
#'   p = c(9, 12, 4) / 25,
#'   q = c(1,  1, 1) / 3
#' )
#'
#' info_kl_divergence_matrix(wikipedia_example)
info_surprisal <- function(p) {
  -log(p)
}

#' @rdname information
#' @export
info_cross_entropy <- function(p, q) {
  stopifnot(
    "Probabilities must sum to 1" = all.equal(sum(p), 1),
    "Probabilities must sum to 1" = all.equal(sum(q), 1)
  )
  sum(p * info_surprisal(q))
}

#' @rdname information
#' @export
info_entropy <- function(p) {
  info_cross_entropy(p, p)
}

#' @rdname information
#' @export
info_kl_divergence <- function(p, q) {
  stopifnot(
    "Probabilities must sum to 1" = all.equal(sum(p), 1),
    "Probabilities must sum to 1" = all.equal(sum(q), 1)
  )
  sum(p * log(p / q))
}

#' @rdname information
#' @export
info_kl_divergence_matrix <- function(mat) {
  kl_row <- Vectorize(
    function(i, j) info_kl_divergence(mat[i, ], mat[j, ])
  )
  m <- outer(1:nrow(mat), 1:nrow(mat), kl_row)
  new_names <- dimnames(mat)
  new_names[[2]] <- new_names[[1]]
  dimnames(m) <- new_names
  m
}




# info_kl_divergence <- function(p, q, epsilon = 1e-6) {
#   stopifnot(
#     "Probabilities must sum to 1" = all.equal(sum(p), 1),
#     "Probabilities must sum to 1" = all.equal(sum(q), 1)
#   )
#   p <- p + epsilon
#   q <- q + epsilon
#   sum(p * log(p / q))
# }
#

