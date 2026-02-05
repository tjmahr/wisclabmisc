


#' A lightweight container for auditing or logging
#' @param data data to wrap into an audit
#' @param x an audit object
#' @param fn a function to apply to the data inside of the audit object
#' @param ... further arguments for `fn`
#' @export
#' @rdname audit
#' @description
#' An audit is a `list()` with two fields:
#'
#' 1. `data` the most recently updated (or poked) data
#' 2. `notes` a history of the `fn` calls and results applied to the object.
#'
#' Usage:
#'
#' - `audit_wrap(data)` to wrap some data inside of an audit.
#' - `audit_peek(x, fn, ...)` to apply a function to the data, log the results
#'   in the `notes`, but *not* overwrite the audit data.
#' - `audit_poke(x, fn, ...)` to apply a function to the data, log the results
#'   in the `notes`, and overwrite the audit data.
#' - `audit_unwrap()` to return the underlying data.
#' @examples
#' x <- letters |>
#'   audit_wrap() |>
#'   audit_peek(rev) |>
#'   audit_peek(toupper)
#'
#' # Note that none of the underlying data has changed. This is useful for
#' # finding values that fail validation checks.
#' x
#'
#' # Use _poke() to update the data:
#' x <- letters |>
#'   audit_wrap() |>
#'   audit_peek(rev) |>
#'   audit_poke(toupper) |>
#'   audit_peek(getElement, 1) |>
#'   audit_peek(function(x) character(0))
#' x
#'
#' # Index into the notes to pull the data
#' x$notes$`getElement(x, 1`$result
audit_wrap <- function(data) {
  structure(list(data = data, notes = list()), class = "audit")
}

#' @export
#' @rdname audit
audit_peek <- function(x, fn, ...) {
  result <- fn(x$data, ...)
  call <- rlang::call2(substitute(fn), quote(x), ...)
  label <- audit_label_call(call, length(x$notes) + 1)

  n <- list(new_audit_note(result, label, "peek"))
  names(n) <- label

  x$notes <- c(x$notes, n)
  x
}

#' @export
#' @rdname audit
audit_poke <- function(x, fn, ...) {
  result <- fn(x$data, ...)
  call <- rlang::call2(substitute(fn), quote(x), ...)
  label <- audit_label_call(call, length(x$notes) + 1)

  n <- list(new_audit_note(result, label, "poke"))
  names(n) <- label

  x$data <- result
  x$notes <- c(x$notes, n)
  x
}

#' @export
#' @rdname audit
audit_unwrap <- function(x) x$data

#' @export
print.audit <- function(x, ...) {
  create_note_name <- function(n) {
    sprintf("%s [%s]", n$label, n$type)
  }
  copy <- x
  copy$notes <- lapply(x$notes, getElement, "result")
  names(copy$notes) <- lapply(x$notes, create_note_name)

  b <- copy["data"] |>
    str(comp.str = "", no.list = TRUE, nest.lev = 0, indent.str = "") |>
    capture.output()

  c <- copy$notes |>
    str(comp.str = "> ", max.level = 1, no.list = TRUE) |>
    capture.output()

  lines <- c("<audit> object", b, "notes:", c)
  writeLines(lines)
  x
}

new_audit_note <- function(result, label, type = c("peek", "poke")) {
  type <- match.arg(type)

  structure(
    list(result = result, type = type, label = label),
    class = "audit_note"
  )
}

audit_label_call <- function(call, unnamed_id) {
  if (!rlang::is_call_simple(call)) {
    f <- paste0("<unnamed-function-", unnamed_id, ">")
    call <- rlang::call2(rlang::sym(f), !!! rlang::call_args(call))
  }
  deparse1(call)
}

