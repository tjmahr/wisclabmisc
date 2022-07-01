
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
#'   (e.g., `"age"`) and outcome variable (.e.g, `"intelligibility"`). These
#'   arguments apply to `fit_beta_gamlss_se()`.
#' @param df_mu,df_sigma degrees of freedom
#' @param control a [gamlss::gamlss.control()] controller. Defaults to `NULL`
#'   which uses default settings, except for setting trace to `FALSE` to silence
#'   the output from gamlss.
#' @return for `fit_beta_gamlss()` and `fit_beta_gamlss_se()`, a
#'   [mem_gamlss()]-fitted model. The `.user` data in the model includes degrees
#'   of freedom for each parameter and the [splines::ns()] basis for each
#'   parameter. For `predict_beta_gamlss()`, a dataframe containing the
#'   model predictions for mu and sigma, plus columns for each centile in
#'   `centiles`. For `optimize_beta_gamlss_slope()`, a dataframe with the
#'   optimized `x` values (`maximum` or `minimum`), the gradient at that `x`
#'   value (`objective`), and the quantile (`quantile`).
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
#' warning message. Therefore, `predict_beta_gamlss()` provides an alternative
#' way to compute centiles from the model. This function manually computes the
#' centiles instead of relying on [gamlss::centiles()]. The main difference is
#' that new *x* values go through [splines::predict.ns()] and then these are
#' multiplied by model coefficients.
#'
#' `optimize_beta_gamlss_slope()` computes the point (i.e., age) and rate of
#' steepest growth for different quantiles. This function wraps over the
#' following process:
#'
#' - an internal prediction function computes a quantile at some `x` from model
#'   coefficients and spline bases.
#' - another internal function uses `numDeriv::grad()` to get the gradient
#'   of this prediction function for `x`.
#' - `optimize_beta_gamlss_slope()` uses `stats::optimize()` on the gradient
#'   function to find the `x` with the maximum or minimum slope.
#'
#' ## GAMLSS does beta regression differently
#'
#' This part is a brief note that GAMLSS uses a different parameterization of the
#' beta distribution for its beta family than other packages.
#'
#' The canonical parameterization of the beta distribution uses shape parameters
#' \eqn{\alpha} and \eqn{\beta} and the probability density function:
#'
#' \deqn{f(y;\alpha,\beta) = \frac{1}{B(\alpha,\beta)} y^{\alpha-1}(1-y)^{\beta-1}}
#'
#' where \eqn{B} is the [beta
#' function](https://en.wikipedia.org/wiki/Beta_function).
#'
#' For beta regression, the distribution is reparameterized so that there is a
#' mean probability \eqn{\mu} and some other parameter that represents the
#' spread around that mean. In GAMLSS ([gamlss.dist::BE()]), they use a scale
#' parameter \eqn{\sigma} (larger values mean more spread around mean).
#' Everywhere else---[betareg::betareg()] and [rstanarm::stan_betareg()] in
#' `vignette("betareg", "betareg")`, [brms::Beta()] in
#' `vignette("brms_families", "brms")`, [mgcv::betar()]---it's a precision
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
#' # But predict_gen_gamma_gamlss() does this work for us and also provides
#' # centiles
#' new_ages <- data.frame(age_months = 48:71)
#' centiles <- predict_beta_gamlss(new_ages, m)
#' centiles
#'
#' # Confirm that the manual prediction matches the automatic one
#' centiles[centiles$age_months == 55, "mu"]
#' stats::plogis(logit_mean_55)
#'
#' if(requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   ggplot(pivot_centiles_longer(centiles)) +
#'     aes(x = age_months, y = .value) +
#'     geom_line(aes(group = .centile, color = .centile_pair)) +
#'     geom_point(
#'       aes(y = intelligibility),
#'       data = subset(
#'         data_fake_intelligibility,
#'         48 <= age_months & age_months <= 71
#'       )
#'     )
#' }
#'
#' # Age of steepest growth for each centile
#' optimize_beta_gamlss_slope(
#'   model,
#'   centiles = c(5, 10, 50, 90),
#'   interval = range(data_fake_intelligibility$age_months)
#' )
#'
#' # Manual approach: Make fine grid of predictions and find largest jump
#' centiles <- predict_beta_gamlss(
#'   data.frame(age_months = seq(28, 95, length.out = 1000)),
#'   model
#' )
#' centiles[which.max(diff(centiles$c5)), "age_months"]
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



#' @rdname beta-intelligibility
#' @param model a model fitted by [fit_beta_gamlss()]
#' @inheritParams predict_centiles
#' @export
predict_beta_gamlss <- function(
  newdata,
  model,
  centiles = c(5, 10, 50, 90, 95)
) {
  stopifnot(ncol(newdata) == 1)
  newx <- newdata[[1]]
  params <- c("mu", "sigma")
  g <- gamlss.dist::BE()
  list_results <- list()

  for (param in params) {
    basis <- model$.user[[paste0("basis_", param)]]
    basis_w_intercept <- cbind(1, stats::predict(basis, newx = newx))
    f <- g[[paste0(param, ".linkinv")]]

    result <- basis_w_intercept %*% stats::coef(model, what = param)
    list_results[[param]] <- f(result[, 1])
  }

  list_centiles <- centiles |>
    lapply(
      function(x) {
        gamlss.dist::qBE(
          x / 100,
          list_results$mu,
          list_results$sigma,
        )
      }
    ) |>
    stats::setNames(paste0("c", centiles))

  dplyr::bind_cols(newdata, list_results, list_centiles) |>
    tibble::as_tibble()
}


#' @rdname beta-intelligibility
#' @inheritParams predict_beta_gamlss
#' @param interval for `optimize_beta_gamlss_slope()`, the range of x values to
#'   optimize over.
#' @param maximum for `optimize_beta_gamlss_slope()`, whethre to find the
#'   maximum slope (`TRUE`) or minimum slope (`FALSE`).
#' @export
optimize_beta_gamlss_slope <- function(
  model,
  centiles = 50,
  interval = c(30, 119),
  maximum = TRUE,
  ...
) {
  quantiles <- centiles / 100

  coef_mu <- stats::coef(model, "mu")
  basis_mu <- model$.user$basis_mu

  coef_sigma <- stats::coef(model, "sigma")
  basis_sigma <- model$.user$basis_sigma

  results <- as.list(quantiles) |>
    lapply(
      function(q) {
        o <- stats::optimize(
          gradient_beta_gamlss,
          interval = interval,
          maximum = maximum,
          quantile = q,
          coef_mu = coef_mu,
          coef_sigma = coef_sigma,
          basis_mu = basis_mu,
          basis_sigma = basis_sigma
        )
        o$quantile <- q
        tibble::as_tibble(o)
      }
  )
  dplyr::bind_rows(results)
}

gradient_beta_gamlss <- function(
  xs,
  quantile,
  coef_mu,
  coef_sigma,
  basis_mu,
  basis_sigma
) {
  numDeriv::grad(
    .predict_beta_gamlss_quantile,
    # gradient computed with respect to x
    x = xs,
    quantile = quantile,
    coef_mu = coef_mu,
    coef_sigma = coef_sigma,
    basis_mu = basis_mu,
    basis_sigma = basis_sigma
  )
}


# Predict quantiles from model coefficients
.predict_beta_gamlss_quantile <- function(
  xs,
  quantile,
  coef_mu,
  coef_sigma,
  basis_mu,
  basis_sigma,
  target = 0
) {
  inv_logit <- stats::plogis

  mat_mu <- cbind(1, predict(basis_mu, xs))
  mat_sigma <- cbind(1, predict(basis_sigma, xs))

  mu <- inv_logit(mat_mu %*% coef_mu)
  sigma2 <- inv_logit(mat_sigma %*% coef_sigma) ^ 2

  alpha <- (mu - mu * sigma2) / sigma2
  beta <- ((mu - 1) * (sigma2 - 1)) / sigma2

  stats::qbeta(quantile, alpha, beta) - target
}
