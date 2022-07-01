



try_uniroot <- function(...) {
  tryCatch(
    stats::uniroot(...),
    error = function(e) {
      list(
        root = NA,
        f.root = NA,
        iter = NA,
        init.it = NA,
        estim.prec = NA
      )
    }
  )
}

pmap_uniroot_predict_quantile <- function(
    model,
    # coef_samples,
    centiles = c(50),
    targets,
    interval = c(30, 119)
) {
  basis_mu <- model$.user$basis_mu
  basis_sigma <- model$.user$basis_sigma



  try_uniroot(
    .predict_beta_gamlss_quantile,
    interval = interval,
    quantile = quantile,
    coef_mu = coef_mu,
    coef_sigma = coef_sigma,
    basis_mu = basis_mu,
    basis_sigma = basis_sigma,
    target = target
  )


  # knots_mu <- attr(basis_mu, "knots")
  # knots_sigma <- attr(basis_sigma, "knots")
  # boundary_knots_mu <- attr(basis_mu, "Boundary.knots")
  # boundary_knots_sigma <- attr(basis_sigma, "Boundary.knots")
  #
  # m <- coef_samples



  # do_this_uniroot <- function(
    #   quantile,
  #   constant_mu,
  #   constant_sigma,
  #   beta_mu,
  #   beta_sigma,
  #   knots_mu,
  #   boundary_knots_mu,
  #   knots_sigma,
  #   boundary_knots_sigma,
  #   target
  # ) {


  setup <- tidyr::expand_grid(
    .draw = seq_len(nrow(m$draws_mu)),
    .quantile = quantiles,
    .target = targets
  ) %>%
    mutate(
      constant_mu    = map(.draw, ~ m$draws_mu[.x, 1]) ,
      constant_sigma = map(.draw, ~ m$draws_sigma[.x, 1]) ,
      beta_mu    = map(.draw, ~ m$draws_mu[.x, -1]) ,
      beta_sigma = map(.draw, ~ m$draws_sigma[.x, -1])
    )

  setup %>%
    mutate(
      results = furrr::future_pmap(
        list(
          quantile = .quantile,
          constant_mu = constant_mu,
          constant_sigma = constant_sigma,
          beta_mu = beta_mu,
          beta_sigma = beta_sigma,
          target = .target
        ),
        do_this_uniroot,
        knots_mu = knots_mu,
        boundary_knots_mu = boundary_knots_mu,
        knots_sigma = knots_sigma,
        boundary_knots_sigma = boundary_knots_sigma
      ),
      results = lapply(results, as_tibble)
    ) %>%
    unnest(results) %>%
    select(.draw, .quantile, .target, root:estim.prec)
}
