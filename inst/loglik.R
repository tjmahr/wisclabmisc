data <- mtcars
m1 <- lm(mpg ~ wt, data)
m2 <- lm(mpg ~ wt * factor(cyl), data)

# stats::sigma() does a 1 / (n - p) correction
naive_sigma <- function(model) {
  r <- residuals(model)
  n <- length(r)
  sqrt(mean(r ^ 2))
}

l1 <- dnorm(mtcars$mpg, predict(m1), naive_sigma(m1))
l2 <- dnorm(mtcars$mpg, predict(m2), naive_sigma(m2))

logLik(m1)
-sum(info_surprisal(l1))

logLik(m2)
-sum(info_surprisal(l2))

# i think i need the extract parameter for the sigma
2 * sum(info_surprisal(l2)) + 2 * (length(coef(m2)) + 1)
2 * sum(info_surprisal(l2)) + 2 * attr(logLik(m2), "df")
AIC(m2)


p <- l1 / (sum(l1))
q <- l2 / (sum(l2))
info_entropy(p)
info_entropy(q)

info_kl_divergence(p, q)
info_kl_divergence(q, p)
