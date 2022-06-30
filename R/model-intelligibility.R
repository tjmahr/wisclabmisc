
utils::globalVariables(c("BE"))


#' Fit a beta regression model (for intelligibility)
#'
#' The function fits the same type of GAMLSS model as used in [Hustad and
#' colleagues (2021)](https://doi.org/10.1044/2021_JSLHR-21-00142) 🔓:
#' A beta regression model (via [gamlss.dist::BE()]) with natural cubic splines
#' on the mean (mu) and scale (sigma). This model is fitted using this package's
#' [mem_gamlss()] wrapper function.
#'
#' @rdname beta-intelligibility
#' @param data a data frame
#' @param var_x,var_y (unquoted) variable names giving the predictor variable
#'   (e.g., `age`) and outcome variable (.e.g, `intelligibility`).
#' @param name_x,name_y quoted variable names giving the predictor variable
#'   (e.g., `"age"`) and outcome variable (.e.g, `"intelligibility"`). These arguments
#'   apply to `fit_beta_gamlss_se()`.
#' @param df_mu,df_sigma degrees of freedom
#' @param control a [gamlss::gamlss.control()] controller. Defaults to `NULL`
#'   which uses default settings, except for setting trace to `FALSE` to silence
#'   the output from gamlss.
#' @return for `fit_beta_gamlss()` and `fit_beta_gamlss_se()`, a
#'   [mem_gamlss()]-fitted model. The `.user` data in the model includes degrees
#'   of freedom for each parameter and the [splines::ns()] basis for each
#'   parameter. For `predict_beta_gamlss()`, a dataframe containing the
#'   model predictions for mu and sigma, plus columns for each centile in
#'   `centiles`.
#' @export
#' @details
#'
#' There are two versions of this function. The main version is
#' `fit_beta_gamlss()`, and it works with unquoted column names (e.g.,
#' `age`). The alternative version is `fit_beta_gamlss_se()`; the final
#' "se" stands for "Standard Evaluation". This designation means that the
#' variable names must be given as strings (so, the quoted `"age"` instead of
#' bare name `age`). This alternative version is necessary when we fit several
#' models using parallel computing with [furrr::future_map()] (as when using
#' bootstrap resampling).
#'
#' [predict_centiles()] will work with this function, but it will likely throw a
#' warning message. Therefore, `predict_beta_gamlss()` provides an
#' alternative way to compute centiles from the model. It manually computes the
#' centiles instead of relying on [gamlss::centiles()].
#'
#' ## GAMLSS does beta regression differently
#'
#' This part is a brief note that GAMLSS uses a different parameterization of the
#' beta distribution for its beta family than other packages.
#'
#' The canonical parameterization of the beta distribution uses shape parameters
#' \eqn{\alpha} and \eqn{\beta} and the probability density function:
#'
#' \deqn{f(y;\alpha,\beta) = \frac{1}{\Beta(\alpha,\beta)} y^{\alpha-1}(1-y)^{\beta-1}}
#'
#' where \eqn{\Beta} is the [beta
#' function](https://en.wikipedia.org/wiki/Beta_function).
#'
#' For beta regression, the distribution is reparameterized so that there is a
#' mean probability \eqn{\mu} and some other parameter that represents the
#' spread around that mean. In GAMLSS (`gamlss.dist::BE()`), they use a scale
#' parameter \eqn{\sigma} (larger values mean more spread around mean).
#' Everywhere else---[betareg::betareg()] and [rstanarm::stan_betareg()] in
#' `vignette("betareg", "betareg")`, [brms::Beta()] in
#' `vignette("brms_families", "brms")``, [mgcv::betar()]---it's a precision
#' parameter \eqn{\phi} (larger values mean more precision, less spread around
#' mean). Here is a comparison:
#'
#'\deqn{
#'  \text{betareg, brms, mgcv, etc.} \\
#'  \mu              = \alpha / (\alpha + \beta) \\
#'  \phi             = \alpha + b \\
#'  \textsf{E}(y)    = \mu \\
#'  \textsf{VAR}(y)  = \mu(1-\mu)/(1 + \phi) \\
#'}
#'
#'\deqn{
#'  \text{GAMLSS} \\
#'  \mu             = \alpha / (\alpha + \beta) \\
#'  \sigma          = (1 / (\alpha + \beta + 1))^.5 \\
#'  \textsf{E}(y)   = \mu \\
#'  \textsf{VAR}(y) = \mu(1-\mu)\sigma^2
#'}
#'
#' @concept models
#' @source Associated article: <https://doi.org/10.1044/2021_JSLHR-21-00142>
#' @examples
#' data_fake_intelligibility
#'
#' m <- fit_beta_gamlss(
#'   data_fake_intelligibility,
#'   age_months,
#'   intelligibility
#' )
#'
#' # using "qr" in summary() just to suppress a warning message
#' summary(m, type = "qr")
#'
#' # Alternative interface
#' d <- data_fake_intelligibility
#' m2 <- fit_beta_gamlss_se(
#'   data = d,
#'   name_x = "age_months",
#'   name_y = "intelligibility"
#' )
#' coef(m2) == coef(m)
#'
#' # how to use control to change gamlss() behavior
#' m_traced <- fit_beta_gamlss(
#'   data_fake_intelligibility,
#'   age_months,
#'   intelligibility,
#'   control = gamlss::gamlss.control(n.cyc = 15, trace = TRUE)
#' )
#'
#' # The `.user` space includes the spline bases, so that we can make accurate
#' # predictions of new xs.
#' names(m$.user)
#'
#' # predict logit(mean) at 55 months:
#' logit_mean_55 <- cbind(1, predict(m$.user$basis_mu, 55)) %*% coef(m)
#' logit_mean_55
#' stats::plogis(logit_mean_55)
#'
# # But predict_gen_gamma_gamlss() does this work for us and also provides
# # centiles
# new_ages <- data.frame(age_months = 48:71)
# centiles <- predict_gen_gamma_gamlss(new_ages, m)
# centiles
#
# # Confirm that the manual prediction matches the automatic one
# centiles[centiles$age_months == 55, "mu"]
# exp(log_mean_55)
#
# if(requireNamespace("ggplot2", quietly = TRUE)) {
#   library(ggplot2)
#   ggplot(pivot_centiles_longer(centiles)) +
#     aes(x = age_months, y = .value) +
#     geom_line(aes(group = .centile, color = .centile_pair)) +
#     geom_point(
#       aes(y = speaking_sps),
#       data = subset(
#         data_fake_rates,
#         48 <= age_months & age_months <= 71
#       )
#     )
# }
fit_beta_gamlss <- function(
    data,
    var_x,
    var_y,
    df_mu = 3,
    df_sigma = 2,
    control = NULL
) {
  # See link for a guide on the nonstandard evaluation used here
  # https://adv-r.hadley.nz/evaluation.html#wrapping-modelling-functions
  rlang::check_installed(
    pkg = c("gamlss.dist", "splines", "gamlss"),
    reason = "for fit_beta_gamlss()"
  )

  # We need these symbols available for the model
  model_env <- rlang::env(
    # what the user can see
    rlang::caller_env(),
    # what we need
    BE = gamlss.dist::BE,
    gamlss.control = gamlss::gamlss.control,
    ns = splines::ns,
    mem_gamlss = mem_gamlss
  )

  if (is.null(control)) {
    control <- gamlss::gamlss.control(trace = FALSE)
  }

  # Stash the symbols the user provided
  var_y <- enexpr(var_y)
  var_x <- enexpr(var_x)

  # Build the formula by plugging the variable names and degrees of freedom
  ns_mu    <- expr(ns(!!var_x, df = !! df_mu))
  ns_sigma <- expr(ns(!!var_x, df = !! df_sigma))

  f_mu    <- rlang::new_formula(expr(!!var_y), ns_mu,    env = model_env)
  f_sigma <- rlang::new_formula(NULL,          ns_sigma, env = model_env)

  # We could fit the model directly but the user would see `f_mu` instead of
  # the actual formula used so we need to create a call.
  model_call <- expr(
    mem_gamlss(
      !! f_mu,
      sigma.formula = !! f_sigma,
      family = BE(),
      data = {{ data }},
      control = !! control
    )
  )
  model <- rlang::eval_tidy(model_call, env = model_env)

  model$.user$df_mu    <- df_mu
  model$.user$df_sigma <- df_sigma

  # Actually evaluated the spline formula
  d <- rlang::eval_tidy(data)
  model$.user$basis_mu    <- rlang::eval_tidy(ns_mu, d, env = model_env)
  model$.user$basis_sigma <- rlang::eval_tidy(ns_sigma, d, env = model_env)
  model
}



#' @rdname beta-intelligibility
#' @export
fit_beta_gamlss_se <- function(
    data,
    name_x,
    name_y,
    df_mu = 3,
    df_sigma = 2,
    control = NULL
) {
  # Standard evaluation version for future-based parallelism, see
  # https://furrr.futureverse.org/articles/gotchas.html

  # Convert the strings to symbols but tunnel `data` straight through
  var_x <- ensym(name_x)
  var_y <- ensym(name_y)
  fit_beta_gamlss(
    {{ data }}, !! var_x, !! var_y, df_mu, df_sigma, control
  )
}
