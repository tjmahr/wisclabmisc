
#' Compute sensitivity and specificity scores from (weighted) data
#'
#' @inheritParams compute_empirical_roc
#' @param weights a bare column name for the observation weights. If `weights`
#'   is set to `NULL` or not provided, all values receive equal weight and
#'   regular sensivity and specificity scores are used.
#' @param direction `direction` for computing case status. Provide `"low"`
#' if a low score predicts case status (`case <= threshold < control`).
#' Provide `"high"` if a high score predicts case status.
#' (`control < threshold <= case`). pROC's `direction` conventions are also
#' supported where `"<"` (as in `control < case`) corresponds to `"high"`, and
#' `">"` (as in `control > case`) corresponds to `"low"`.
#' @param levels two-element vector where the first element is the label for the
#'   control group and the second element is the label for the case group.
#'
#' `pROC::roc()` does not support observation-weights when computing ROC curves
#' so this function computes
#' @concept roc
compute_sens_spec_from_ecdf <- function(
    data,
    response,
    predictor,
    weights = NULL,
    direction = NULL,
    levels = NULL
) {
  direction <- "low"

  # testing with literal weights
  data <- mtcars
  levels <- c("4", "6")
  q_response <- quote(cyl)
  q_predictor <- quote(hp)
  direction <- "low"

  if (is.null(direction)) {
    stop("Provide a `direction`")
  } else if (direction == "<") {
    direction <- "high"
  } else if (direction == ">") {
    direction <- "low"
  }

  q_response <- enquo(response)
  q_predictor <- enquo(predictor)
  q_weights <- enquo(weights)
  q_weights <- quote(wt)
  q_weights <- quo(wt)
  x <- select(data, !! q_predictor)
  y <- select(data, !! q_response)

  d <- data |> filter(is.element(!! q_response, levels))
  current_levels <- d |> pull(q_response) |> unique()
  stopifnot(length(current_levels) == 2)

  x1 <- d |>
    filter(is.element(!! q_response, levels[1])) |>
    pull(!! q_predictor)
  x2 <- d |>
    filter(is.element(!! q_response, levels[2])) |>
    pull(!! q_predictor)

  if (!rlang::quo_is_null(q_weights)) {
    w1 <- d |>
      filter(is.element(!! q_response, levels[1])) |>
      pull(!! q_weights)
    w2 <- d |>
      filter(is.element(!! q_response, levels[2])) |>
      pull(!! q_weights)
  } else {
    w1 <- rep(1, length(x1))
    w2 <- rep(1, length(x2))
  }

  d2 <- .compute_sens_spec(x1, w1, x2, w2, direction)
  names(d2)[names(d2) == "x"] <- colnames(x)[1]
  d2$.direction <- direction
  d2$.response <- colnames(y)[1]
  d2$.controls <- levels[1]
  d2$.cases <- levels[2]

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


}




.compute_sens_spec <- function(x1, w1, x2, w2, direction) {
  d <- data.frame(
    x = c(x1, x2, -Inf, Inf),
    w = c(w1, w2, 0, 0),
    in_x1 = c(rep(1, length(x1)), rep(0, length(x2)), 0, 0),
    in_x2 = c(rep(0, length(x1)), rep(1, length(x2)), 0, 0)
  )
  d1 <- d |>
    arrange(x) |>
    group_by(x) |>
    summarise(
      n_no = sum(in_x1),
      n_yes = sum(in_x2),
      w_no = sum(in_x1 * w),
      w_yes = sum(in_x2 * w),
    ) |>
    arrange(x) |>
    mutate(
      .sensitivities = cumsum(w_yes) / sum(w_yes),
      .specificities = 1 - (cumsum(w_no) / sum(w_no))
    )

  if (direction == "high") {
    d1$.sensitivities <- 1 - d1$.sensitivities
    d1$.specificities <- 1 - d1$.specificities
  }

  d1
}
