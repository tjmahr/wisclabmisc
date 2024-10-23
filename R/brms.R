
#' Set default arguments for brms model fitting
#' @param ... arguments to [brms::brm()] to use as default values.
#' @param .backend,.threads,.chains,.cores,.iter,.silent,.file_refit,.refresh,.control
#'   These arguments set *default* default values. Overwrite these defaults by
#'   using the argument without the `.` prefix.
#' @return A function for generating a list of arguments to [brms::brm()].
#' @export
#'
#' @details
#' One can set the default value for `adapt_delta` directly
#' (`adapt_delta = .98`). This value will be propagated to
#' `control$adapt_delta`.
#'
#' @examples
#' brm_args <- brms_args_create()
#'
#' # using package-provided defaults
#' brm_args()
#'
#' # overwriting a default value
#' brm_args(iter = 500)
#'
#' # adapt_delta is handled specially
#' brm_args(adapt_delta = .95)
#' # adapt_delta is handled specially
#' brm_args(adapt_delta = .95)
#'
#' # We can overwrite the package-provided defaults
#' other_brm_args <- brms_args_create(iter = 4000, backend = "rstan")
#' other_brm_args()
#'
#' # And overwrite them too
#' other_brm_args(backend = "cmdstanr")
brms_args_create <- function(
    ...,
    .backend = "cmdstanr",
    .threads = 2,
    .chains = 4,
    .cores = 4,
    .iter = 2000,
    .silent = 0,
    .file_refit = "on_change",
    .refresh = 25,
    .control = list()
  ) {
  # the .names prevent `file` from partial matching `file_refit`
  defaults <- list(
    backend = .backend,
    threads = .threads,
    chains = .chains,
    cores = .cores,
    iter = .iter,
    silent = .silent,
    file_refit = .file_refit,
    refresh = .refresh,
    control = .control
  )
  outer_dots <- list(...) |>
    handle_control_args()

  # Let the user set adapt_delta outside of a `control` argument

  defaults <- utils::modifyList(defaults, outer_dots)

  function(...) {
    inner_dots <- list(...) |> handle_control_args()
    l <- utils::modifyList(defaults, inner_dots)
    structure(l, class = c("brm_args", "list"))
  }
}

#' @export
print.brm_args <- function(x, ...) {
  utils::str(x, ...)
}

handle_control_args <- function(args) {
  if (!is.null(args$adapt_delta)) {
    if(is.null(args$control)) {
      args$control <- list()
    }
    args$control$adapt_delta <- args$adapt_delta
    args$adapt_delta <- NULL
  }
  args
}
