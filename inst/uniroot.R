data_fake_intelligibility

m <- fit_beta_gamlss(
  data_fake_intelligibility,
  age_months,
  intelligibility
)

# using "qr" in summary() just to suppress a warning message
summary(m, type = "qr")

# Alternative interface
d <- data_fake_intelligibility
m2 <- fit_beta_gamlss_se(
  data = d,
  name_x = "age_months",
  name_y = "intelligibility"
)
coef(m2) == coef(m)























# how to use control to change gamlss() behavior
m_traced <- fit_beta_gamlss(
  data_fake_intelligibility,
  age_months,
  intelligibility,
  control = gamlss::gamlss.control(n.cyc = 15, trace = TRUE)
)







model <- m
pmap_uniroot_predict_quantile <- function(
    model,
    centiles = 50,
    targets = .5,
    interval = c(30, 119)
) {
  centiles <- c(10, 50, 90)
  targets <- c(.5, .6, .25)

#
#
#   setup <- tidyr::expand_grid(
#     .draw = seq_len(nrow(m$draws_mu)),
#     .quantile = quantiles,
#     .target = targets
#   ) %>%
#     mutate(
#       constant_mu    = map(.draw, ~ m$draws_mu[.x, 1]) ,
#       constant_sigma = map(.draw, ~ m$draws_sigma[.x, 1]) ,
#       beta_mu    = map(.draw, ~ m$draws_mu[.x, -1]) ,
#       beta_sigma = map(.draw, ~ m$draws_sigma[.x, -1])
#     )
#
#   setup %>%
#     mutate(
#       results = furrr::future_pmap(
#         list(
#           quantile = .quantile,
#           constant_mu = constant_mu,
#           constant_sigma = constant_sigma,
#           beta_mu = beta_mu,
#           beta_sigma = beta_sigma,
#           target = .target
#         ),
#         do_this_uniroot,
#         knots_mu = knots_mu,
#         boundary_knots_mu = boundary_knots_mu,
#         knots_sigma = knots_sigma,
#         boundary_knots_sigma = boundary_knots_sigma
#       ),
#       results = lapply(results, as_tibble)
#     ) %>%
#     unnest(results) %>%
#     select(.draw, .quantile, .target, root:estim.prec)
}
