utils::globalVariables(c("ns", "GG", "gamlss.control"))


#' Fit a generalized gamma regression model (for speaking rate)
#'
#' The function fits the same type of GAMLSS model as used in [Mahr
#' 2021](https://doi.org/10.1044/2021_JSLHR-21-00206) 🔓: A generalized gamma
#' regression model (via [gamlss.dist::GG()]) with
#' natural cubic splines on the mean (mu), scale (sigma),
#' and shape (nu) of the distribution. This model is fitted using this
#' package's [mem_gamlss()] wrapper function.
#'
#' @rdname gen-gamma-rate
#' @param data a data frame
#' @param var_x,var_y (unquoted) variable names giving the predictor variable
#'   (e.g., `age`) and outcome variable (.e.g, `rate`).
#' @param name_x,name_y quoted variable names giving the predictor variable
#'   (e.g., `"age"`) and outcome variable (.e.g, `"rate"`). These arguments
#'   apply to `fit_gen_gamma_gamlss_se()`.
#' @param df_mu,df_sigma,df_nu degrees of freedom
#' @param control a [gamlss::gamlss.control()] controller. Defaults to `NULL`
#'   which uses default settings, except for setting trace to `FALSE` to silence
#'   the output from gamlss.
#' @return for `fit_gen_gamma_gamlss()` and `fit_gen_gamma_gamlss_se()`, a
#'   [mem_gamlss()]-fitted model. The `.user` data in the model includes degrees
#'   of freedom for each parameter and the [splines::ns()] basis for each
#'   parameter. For `predict_gen_gamma_gamlss()`, a dataframe containing the
#'   model predictions for mu, sigma, and nu, plus columns for each centile in
#'   `centiles`.
#' @export
#' @details
#'
#' There are two versions of this function. The main version is
#' `fit_gen_gamma_gamlss()`, and it works with unquoted column names (e.g.,
#' `age`). The alternative version is `fit_gen_gamma_gamlss_se()`; the final
#' "se" stands for "Standard Evaluation". This designation means that the
#' variable names must be given as strings (so, the quoted `"age"` instead of
#' bare name `age`). This alternative version is necessary when we fit several
#' models using parallel computing with [furrr::future_map()] (as when using
#' bootstrap resampling).
#'
#' [predict_centiles()] will work with this function, but it will likely throw a
#' warning message. Therefore, `predict_gen_gamma_gamlss()` provides an
#' alternative way to compute centiles from the model. It manually computes the
#' centiles instead of relying on [gamlss::centiles()].
#'
#' @concept models
#' @source Associated article: <https://doi.org/10.1044/2021_JSLHR-21-00206>
#' @examples
#' data_fake_rates
#'
#' m <- fit_gen_gamma_gamlss(data_fake_rates, age_months, speaking_sps)
#'
#' # using "qr" in summary() just to suppress a warning message
#' summary(m, type = "qr")
#'
#' # Alternative interface
#' d <- data_fake_rates
#' m2 <- fit_gen_gamma_gamlss_se(
#'   data = d,
#'   name_x = "age_months",
#'   name_y = "speaking_sps"
#' )
#' coef(m2) == coef(m)
#'
#' # how to use control to change gamlss() behavior
#' m_traced <- fit_gen_gamma_gamlss(
#'   data_fake_rates,
#'   age_months,
#'   speaking_sps,
#'   control = gamlss::gamlss.control(n.cyc = 15, trace = TRUE)
#' )
#'
#' # The `.user` space includes the spline bases, so that we can make accurate
#' # predictions of new xs.
#' names(m$.user)
#'
#' # predict log(mean) at 55 months:
#' log_mean_55 <- cbind(1, predict(m$.user$basis_mu, 55)) %*% coef(m)
#' log_mean_55
#' exp(log_mean_55)
#'
#' # But predict_gen_gamma_gamlss() does this work for us and also provides
#' # centiles
#' new_ages <- data.frame(age_months = 48:71)
#' centiles <- predict_gen_gamma_gamlss(new_ages, m)
#' centiles
#'
#' # Confirm that the manual prediction matches the automatic one
#' centiles[centiles$age_months == 55, "mu"]
#' exp(log_mean_45)
#'
#' if(requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   ggplot(pivot_centiles_longer(centiles)) +
#'     aes(x = age_months, y = .value) +
#'     geom_line(aes(group = .centile, color = .centile_pair)) +
#'     geom_point(
#'       aes(y = speaking_sps),
#'       data = subset(
#'         data_fake_rates,
#'         48 <= age_months & age_months <= 71
#'       )
#'     )
#' }
fit_gen_gamma_gamlss <- function(
  data,
  var_x,
  var_y,
  df_mu = 3,
  df_sigma = 2,
  df_nu = 1,
  control = NULL
) {
  # See link for a guide on the nonstandard evaluation used here
  # https://adv-r.hadley.nz/evaluation.html#wrapping-modelling-functions

  rlang::check_installed(
    pkg = c("gamlss.dist", "splines", "gamlss"),
    reason = "for fit_gen_gamma_gamlss()"
  )

  # We need these symbols available for the model
  model_env <- rlang::env(
    # what the user can see
    rlang::caller_env(),
    # what we need
    GG = gamlss.dist::GG,
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
  ns_nu    <- expr(ns(!!var_x, df = !! df_nu))

  f_mu    <- rlang::new_formula(expr(!!var_y), ns_mu,    env = model_env)
  f_sigma <- rlang::new_formula(NULL,          ns_sigma, env = model_env)
  f_nu    <- rlang::new_formula(NULL,          ns_nu,    env = model_env)

  # We could fit the model directly but the user would see `f_mu` instead of
  # the actual formula used so we need to create a call.
  model_call <- expr(
    mem_gamlss(
      !! f_mu,
      sigma.formula = !! f_sigma,
      nu.formula = !! f_nu,
      family = GG(),
      data = {{ data }},
      control = !! control
    )
  )
  model <- rlang::eval_tidy(model_call, env = model_env)

  model$.user$df_mu    <- df_mu
  model$.user$df_sigma <- df_sigma
  model$.user$df_nu    <- df_nu

  # Actually evaluated the spline formula
  d <- rlang::eval_tidy(data)
  model$.user$basis_mu    <- rlang::eval_tidy(ns_mu, d, env = model_env)
  model$.user$basis_sigma <- rlang::eval_tidy(ns_sigma, d, env = model_env)
  model$.user$basis_nu    <- rlang::eval_tidy(ns_nu, d, env = model_env)
  model
}

#' @rdname gen-gamma-rate
#' @export
fit_gen_gamma_gamlss_se <- function(
  data,
  name_x,
  name_y,
  df_mu = 3,
  df_sigma = 2,
  df_nu = 1,
  control = NULL
) {
  # Standard evaluation version for future-based parallelism, see
  # https://furrr.futureverse.org/articles/gotchas.html

  # Convert the strings to symbols but tunnel `data` straight through
  var_x <- ensym(name_x)
  var_y <- ensym(name_y)
  fit_gen_gamma_gamlss(
    {{ data }}, !! var_x, !! var_y, df_mu, df_sigma, df_nu, control
  )
}

#' @rdname gen-gamma-rate
#' @param model a model fitted by [fit_gen_gamma_gamlss()]
#' @inheritParams predict_centiles
#' @export
predict_gen_gamma_gamlss <- function(
  newdata,
  model,
  centiles = c(5, 10, 50, 90, 95)
) {
  stopifnot(ncol(newdata) == 1)
  newx <- newdata[[1]]

  params <- c("mu", "sigma", "nu")
  g <- gamlss.dist::GG()
  list_results <- list()

  for (param in params) {
    basis <- model$.user[[paste0("basis_", param)]]
    basis_w_intercept <- cbind(1, stats::predict(basis, newx = newx))
    f <- g[[paste0(param, ".linkinv")]]
    list_results[[param]] <- f(basis_w_intercept %*% stats::coef(model, what = param))[, 1]
  }

  list_centiles <- centiles |>
    lapply(
      function(x) {
        gamlss.dist::qGG(
          x / 100,
          list_results$mu,
          list_results$sigma,
          list_results$nu
        )
      }
    ) |>
    stats::setNames(paste0("c", centiles))

  dplyr::bind_cols(newdata, list_results, list_centiles) |>
    tibble::as_tibble()
}
