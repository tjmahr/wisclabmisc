

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

