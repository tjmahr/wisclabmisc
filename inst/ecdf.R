#' @examples
set.seed(100)

# Simulated 3-class dataset
n <- 50
means <- c("A" = 0, "B" = 1, "C" = 2)
y <- sample(c("A", "B", "C"), n, replace = TRUE)
x <- rnorm(n, mean = means[y], sd = 1)
w <- runif(n, 0.5, 2)
df <- data.frame(y, x, w)

# Compare classes "A" (controls) vs "C" (cases)
roc_tbl <- compute_sens_spec_from_ecdf(
  data = df,
  response = y,
  predictor = x,
  weights = w,
  direction = "control-low",
  levels = c("A", "C")
)
roc_tbl2 <- compute_sens_spec_from_ecdf(
  data = df,
  response = y,
  predictor = x,
  weights = NULL,
  direction = "control-low",
  levels = c("A", "C")
)

dplyr::glimpse(roc_tbl)

# Plot the weighted ROC curve
plot(1 - roc_tbl$.specificities, roc_tbl$.sensitivities, type = "l",
     xlab = "False positive rate", ylab = "True positive rate",
     main = "Weighted empirical ROC (A vs C)")

lines(1 - roc_tbl2$.specificities, roc_tbl2$.sensitivities, col = "blue")

r3 <- compute_empirical_roc(
  df |> filter(y %in% c("A","C")),
  y,
  x,
  direction = "control-low"
)

r3$.auc
roc_tbl2
trapezoid_auc(roc_tbl$.specificities, roc_tbl$.sensitivities)
trapezoid_auc(roc_tbl2$.specificities, roc_tbl2$.sensitivities)

lines(1 - r3$.specificities, r3$.sensitivities, col = "orange")




d2 |>
  select(one_of(
    colnames(x)[1],
    ".sensitivities", ".specificities", ".direction",
    ".response", ".controls", ".cases")
  ) |>
  ggplot() +
  aes(x = hp) +
  geom_step(aes(y = .sensitivities)) +
  geom_step(aes(y = .specificities)) +
  geom_point(
    aes(y = 0, x = hp, size = wt),
    data = mtcars |> filter(cyl == "4")
  ) +
  geom_point(
    aes(y = 1, x = hp, size = wt),
    data = mtcars |> filter(cyl == "6")
  ) +
  stat_ecdf(
    aes(x = hp, y = 1 - after_stat(ecdf)),
    data = mtcars |> filter(cyl == "4"),
    color = "orange"
  )  +
  stat_ecdf(
    aes(x = hp, y = after_stat(ecdf)),
    data = mtcars |> filter(cyl == "6"),
    color = "blue"
  )




