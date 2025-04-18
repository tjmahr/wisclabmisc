% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brms.R
\name{brms_args_create}
\alias{brms_args_create}
\title{Set default arguments for brms model fitting}
\usage{
brms_args_create(
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
)
}
\arguments{
\item{...}{arguments to \code{\link[brms:brm]{brms::brm()}} to use as default values.}

\item{.backend, .threads, .chains, .cores, .iter, .silent, .file_refit, .refresh, .control}{These arguments set \emph{default} default values. Overwrite these defaults by
using the argument without the \code{.} prefix.}
}
\value{
A function for generating a list of arguments to \code{\link[brms:brm]{brms::brm()}}.
}
\description{
Set default arguments for brms model fitting
}
\details{
One can set the default value for \code{adapt_delta} directly
(\code{adapt_delta = .98}). This value will be propagated to
\code{control$adapt_delta}.
}
\examples{
brm_args <- brms_args_create()

# using package-provided defaults
brm_args()

# overwriting a default value
brm_args(iter = 500)

# adapt_delta is handled specially
brm_args(adapt_delta = .95)
# adapt_delta is handled specially
brm_args(adapt_delta = .95)

# We can overwrite the package-provided defaults
other_brm_args <- brms_args_create(iter = 4000, backend = "rstan")
other_brm_args()

# And overwrite them too
other_brm_args(backend = "cmdstanr")
}
