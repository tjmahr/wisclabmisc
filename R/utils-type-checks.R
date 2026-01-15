# This file/design is inspired heavily by the rlang type checker standalone
# at https://github.com/r-lib/rlang/blob/HEAD/R/standalone-types-check.R
# installable with: usethis::use_standalone("r-lib/rlang", "types-check")
#
# I made my own version of it because standalone did not have a function for
# checking vectors of numbers.




# arg and call usage cheatsheet:
#
# Top level use of inspect_number() will print f() and yyyyy
#
# f <- function(yyyyy) {
#   inspect_number(yyyyy, min = 0)
# }
# f(-1)
#
# Pass along arg and call in wrapper function to print outer g() and w
#
# assert_nonnegative <- function(z, call = rlang::caller_env(), arg = rlang::caller_arg(z)) {
#   inspect_number(z, min = 0, arg = arg, call = call)
# }
#
# g <- function(w) {
#   assert_nonnegative(w)
# }
#
# g(-1)





#' Inspect numeric input for type, length, and boundary constraints.
#'
#' This helper validates that `x` is a numeric vector satisfying constraints on
#' missingness, infinite values, decimal/whole-number, and minimum/maximum
#' values. On failure it throws an informative error; on success it returns `x`
#' unchanged.
#'
#' `min` and `max` are optional. If either is `NULL`, the corresponding bound
#' check is skipped.
#'
#' Boundary modes:
#' - `"exclusive"` requires `min < x` (lower) / `x < max` (upper)
#' - `"inclusive"` requires `min <= x` (lower) / `x <= max` (upper)
#' - `"tolerant"` allows a small tolerance around the boundary:
#'   `x >= min - tolerance` and `x <= max + tolerance`.
#'
#' @param x Object to inspect.
#' @param min,max Optional numeric bounds. Use `NULL` to skip a bound.
#' @param allow_infinite Logical; whether `Inf`/`-Inf` are allowed.
#' @param allow_missing Logical; whether `NA` or `NaN` values are allowed.
#' @param allow_null Logical; whether `NULL` is allowed.
#' @param allow_decimals Logical; whether non-integer numeric values are allowed.
#' @param allow_vector Logical; whether `length(x) > 1` is allowed.
#' @param min_boundary,max_boundary One of `"inclusive"`, `"exclusive"`,
#'   or `"tolerant"`, controlling how `min`/`max` are enforced.
#' @param tolerance Numeric tolerance used when a boundary mode is `"tolerant"`.
#'   If `NULL`, a default of `.Machine$double.eps^0.5` is used.
#' @param arg Name of the argument (used in error messages).
#' @param call Call to show in errors.
#'
#' @return `x` is returned invisibly on success; otherwise an error is thrown.
#' @noRd
inspect_number <- function(
    x,
    min = NULL,
    max = NULL,
    allow_infinite = FALSE,
    allow_missing = FALSE,
    allow_null = FALSE,
    allow_decimals = TRUE,
    allow_vector = TRUE,
    min_boundary = c("inclusive", "exclusive", "tolerant"),
    max_boundary = c("inclusive", "exclusive", "tolerant"),
    tolerance = NULL,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env()
) {
  min_boundary <- match.arg(min_boundary)
  max_boundary <- match.arg(max_boundary)

  if (is.null(tolerance)) {
    tolerance <- .Machine$double.eps^0.5
  }

  if (is.null(x)) {
    if (!isTRUE(allow_null)) {
      cli::cli_abort("{.arg {arg}} must not be NULL", call = call)
    } else {
      # NULL gets a safe early exit if allowed
      return(invisible(x))
    }
  }

  if (!is.numeric(x)) {
    cli::cli_abort("{.arg {arg}} must be a numeric vector", call = call)
  }

  n <- length(x)
  if (n == 0L) {
    cli::cli_abort("{.arg {arg}} must not be empty", call = call)
  }

  if (!isTRUE(allow_vector) && n != 1L) {
    cli::cli_abort("{.arg {arg}} must be a scalar (length 1)", call = call)
  }

  if (!isTRUE(allow_missing) && anyNA(x)) {
    cli::cli_abort("{.arg {arg}} must not contain missing values", call = call)
  }

  if (!isTRUE(allow_infinite) && any(is.infinite(x), na.rm = TRUE)) {
    cli::cli_abort("{.arg {arg}} must not contain infinite values", call = call)
  }

  x_checkable <- x[is.finite(x)]

  if (!isTRUE(allow_decimals) && length(x_checkable) > 0L) {
    # see ?is.integer()
    is_whole_number <- function(x, tolerance = .Machine$double.eps^0.5) {
      is.integer(x) | abs(x - round(x)) < tolerance
    }
    if (!all(is_whole_number(x_checkable, tolerance))) {
      cli::cli_abort(
        "{.arg {arg}} must contain whole numbers (no decimals)",
        call = call
      )
    }
  }

  if (length(x_checkable) > 0L) {
    oob_lower <- is_oob_lower(x_checkable, min, min_boundary, tolerance)
    oob_upper <- is_oob_upper(x_checkable, max, max_boundary, tolerance)

    if (any(oob_lower)) {
      cli::cli_abort(
        "{.arg {arg}} has values less than {format(min)}",
        call = call
      )
    }

    if (any(oob_upper)) {
      cli::cli_abort(
        "{.arg {arg}} has values greater than {format(max)}",
        call = call
      )
    }
  }

  invisible(x)
}



is_oob_lower <- function(
    x,
    min,
    boundary = c("inclusive", "exclusive", "tolerant"),
    tolerance = .Machine$double.eps^0.5
) {
  if (is.null(min)) return(rlang::rep_along(x, FALSE))
  boundary <- match.arg(boundary)

  switch(
    boundary,
    # [rule]  = [relationship that *breaks* rule]
    exclusive = x <= min,
    inclusive = x <  min,
    tolerant  = x <  (min - tolerance)
  )
}

is_oob_upper <- function(
    x,
    max,
    boundary = c("inclusive", "exclusive", "tolerant"),
    tolerance = .Machine$double.eps^0.5
) {
  if (is.null(max)) return(rlang::rep_along(x, FALSE))
  boundary <- match.arg(boundary)

  switch(
    boundary,
    # [rule]  = [relationship that *breaks* rule]
    exclusive =               max <= x,
    inclusive =               max <  x,
    tolerant  = (max + tolerance) <  x
  )
}

assert_whole_number_scalar <- function(
    x,
    min = NULL,
    max = NULL,
    allow_infinite = FALSE,
    allow_missing = FALSE,
    allow_null = FALSE,
    min_boundary = c("inclusive", "exclusive", "tolerant"),
    max_boundary = c("inclusive", "exclusive", "tolerant"),
    tolerance = NULL,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env()
) {
  min_boundary <- match.arg(min_boundary)
  max_boundary <- match.arg(max_boundary)

  inspect_number(
    x = x,
    min = min,
    max = max,
    allow_infinite = allow_infinite,
    allow_missing = allow_missing,
    allow_null = allow_null,
    allow_decimals = FALSE,
    allow_vector = FALSE,
    min_boundary = min_boundary,
    max_boundary = max_boundary,
    tolerance = tolerance,
    arg = arg,
    call = call
  )

  invisible(x)
}

assert_whole_number_vector <- function(
    x,
    min = NULL,
    max = NULL,
    allow_infinite = FALSE,
    allow_missing = FALSE,
    allow_null = FALSE,
    min_boundary = c("inclusive", "exclusive", "tolerant"),
    max_boundary = c("inclusive", "exclusive", "tolerant"),
    tolerance = NULL,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env()
) {
  min_boundary <- match.arg(min_boundary)
  max_boundary <- match.arg(max_boundary)

  inspect_number(
    x = x,
    min = min,
    max = max,
    allow_infinite = allow_infinite,
    allow_missing = allow_missing,
    allow_null = allow_null,
    allow_decimals = FALSE,
    allow_vector = TRUE,
    min_boundary = min_boundary,
    max_boundary = max_boundary,
    tolerance = tolerance,
    arg = arg,
    call = call
  )

  invisible(x)
}
