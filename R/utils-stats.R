

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
