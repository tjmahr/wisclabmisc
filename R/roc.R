

#' Create an ROC curve from smoothed densities
#'
#' @param data a dataframe containing densities
#' @param controls,cases bare column name for the densities of the control group
#' @param along optional bare column name for the response values
#' @param best_weights weights for computing the best ROC curve points. Defaults
#'   to `c(1, .5)`, which are the defaults used by `pROC::coords()`.
#' @param direction `direction` to set for the for `pROC::roc()`. Defaults to
#'   `"auto"`.
#' @param ... additional arguments. Not used currently.
#' @return the dataframe is updated with new columns for the `.sensitivities`,
#'   `.specificities`, `.auc`, `.roc_row`, `.is_best_youden` and
#'   `.is_best_closest_topleft`.
#'
#' @export
#' @examples
#' set.seed(100)
#' x1 <- rnorm(100, 4, 1)
#' x2 <- rnorm(100, 2, .5)
#' both <- c(x1, x2)
#' steps <- seq(min(both), max(both), length.out = 200)
#' d1 <- dnorm(steps, mean(x1), sd(x1))
#' d2 <- dnorm(steps, mean(x2), sd(x2))
#' data <- tibble::tibble(
#'   y = steps,
#'   d1 = d1,
#'   d2 = d2,
#'   outcome = rbinom(200, 1, prob = 1 - (d1 / (d1 + d2))),
#'   group = ifelse(outcome, "case", "control")
#' )
#' compute_smooth_density_roc(data, d1, d2)
#' compute_smooth_density_roc(data, d1, d2, along = y)
#'
#' # terrible ROC because the response is not present (just the densities)
#' data_shuffled <- data[sample(seq_len(nrow(data))), ]
#' compute_smooth_density_roc(data_shuffled, d1, d2)
#'
#' # sorted along response first: correct AUC
#' compute_smooth_density_roc(data_shuffled, d1, d2, along = y)
#' @concept roc
compute_smooth_density_roc <- function(
    data,
    controls,
    cases,
    along = NULL,
    best_weights = c(1, 0.5),
    direction = "auto",
    ...
) {
  q_controls <- enquo(controls)
  q_cases <- enquo(cases)
  q_along <- enquo(along)

  no_along <- rlang::quo_is_null(q_along)

  old_data <- data

  if (! no_along) {
    data <- arrange(data, !! q_along)
    a <- pull(data, !! q_along)
  }

  x <- pull(data, !! q_controls)
  y <- pull(data, !! q_cases)

  df <- tibble::tibble(
    !! q_controls := c(NA, .env$x, NA),
    !! q_cases    := c(NA, .env$y, NA)
  )

  if (! no_along) {
    df <- mutate(df, !! q_along := c(NA, a, NA))
  }

  roc <- pROC::roc(
    density.controls = x,
    density.cases = y,
    direction = direction
  )

  # Figure out the direction used and how to (re)order the value
  if (direction == "auto") {
    roc_a <- pROC::roc(
      density.controls = x,
      density.cases = y,
      direction = "<"
    )

    if (all(roc$sensitivities == roc_a$sensitivities)) {
      dir <- "<"
    } else {
      dir <- ">"
    }
  } else {
    dir <- direction
  }

  if (dir == "<") {
    f_sort <- function(xs) xs
  } else {
    f_sort <- rev
  }

  join_names <- colnames(df)
  df[[".sensitivities"]] <- f_sort(roc$sensitivities)
  df[[".specificities"]] <- f_sort(roc$specificities)
  df[[".auc"]] <- as.numeric(roc$auc)
  df[[".roc_row"]] <- seq_len(nrow(df))
  df[[".direction"]] <- dir

  tidied <- tidy_best_roc_coords(roc, best_weights = best_weights) |>
    distinct()
  df <- df |>
    left_join(
      tidied,
      by = c(".sensitivities", ".specificities"),
      # there can be multiple sens = 1, spec = 0 rows
      relationship = "many-to-one"
    ) |>
    mutate(
      .is_best_youden = coalesce(.data$.is_best_youden, FALSE),
      .is_best_closest_topleft = coalesce(
        .data$.is_best_closest_topleft,
        FALSE
      )
    )

  old_data |>
    right_join(df, by = join_names, relationship = "one-to-one")
}

#' Create an ROC curve from observed data
#'
#' @param data a dataframe containing responses (groupings) and predictor
#' variable
#' @param response a bare column name with the group status (control vs. cases)
#' @param predictor a bare column name with the predictor to use for
#'   classification
#' @param direction `direction` to set for the for `pROC::roc()`. Defaults to
#'   `"auto"`.
#' @param best_weights weights for computing the best ROC curve points. Defaults
#'   to `c(1, .5)`, which are the defaults used by `pROC::coords()`.
#' @param ... additional arguments passed to `pROC::roc()`.
#' @return a new dataframe of ROC coordinates is returned with columns for the
#'   predictor variable, `.sensitivities`, `.specificities`, `.auc`,
#'   `.direction`, `.controls`, `.cases`, `.is_best_youden` and
#'   `.is_best_closest_topleft`.
#'
#' @export
#' @examples
#' set.seed(100)
#' x1 <- rnorm(100, 4, 1)
#' x2 <- rnorm(100, 2, .5)
#' both <- c(x1, x2)
#' steps <- seq(min(both), max(both), length.out = 200)
#' d1 <- dnorm(steps, mean(x1), sd(x1))
#' d2 <- dnorm(steps, mean(x2), sd(x2))
#' data <- tibble::tibble(
#'   y = steps,
#'   d1 = d1,
#'   d2 = d2,
#'   outcome = rbinom(200, 1, prob = 1 - (d1 / (d1 + d2))),
#'   group = ifelse(outcome, "case", "control")
#' )
#'
#' # get an ROC on the fake data
#' compute_empirical_roc(data, outcome, y)
#' # this guess the cases and controls from the group name and gets it wrong
#' compute_empirical_roc(data, group, y)
#' # better
#' compute_empirical_roc(data, group, y, levels = c("control", "case"))
#' @concept roc
compute_empirical_roc <- function(
    data,
    response,
    predictor,
    direction = "auto",
    best_weights = c(1, 0.5),
    ...
) {
  q_response <- enquo(response)
  q_predictor <- enquo(predictor)

  x <- select(data, !! q_predictor)
  y <- select(data, !! q_response)

  roc <- pROC::roc_(
    data,
    response = colnames(y)[1],
    predictor = colnames(x)[1],
    direction = direction,
    ...
  )

  tidied <- tidy_best_roc_coords(roc, best_weights = best_weights)

  rename_plan <- c(
    .sensitivities = "sensitivity",
    .specificities = "specificity"
  )

  rename_plan_2 <- c("threshold") |>
    stats::setNames(names(x)[1])

  pROC::coords(roc) %>%
    dplyr::rename(tidyselect::all_of(rename_plan)) %>%
    dplyr::mutate(
      .auc = as.numeric(roc$auc),
      .direction = roc$direction,
      .controls = roc$levels[1],
      .cases = roc$levels[2],
    ) %>%
    dplyr::left_join(
      dplyr::distinct(tidied),
      by = c(".sensitivities", ".specificities", "threshold"),
      relationship = "many-to-one"
    ) %>%
    dplyr::mutate(
      .is_best_youden = dplyr::coalesce(.data$.is_best_youden, FALSE),
      .is_best_closest_topleft = dplyr::coalesce(
        .data$.is_best_closest_topleft,
        FALSE
      )
    ) %>%
    dplyr::rename(tidyselect::all_of(rename_plan_2)) %>%
    tibble::as_tibble()
}


tidy_best_roc_coords <- function(x, best_weights = c(1, 0.5)) {
  coords <- pROC::coords(x, "all", transpose = FALSE)

  youden <- x |>
    pROC::coords(
      ret = c("specificity", "sensitivity"),
      x = "best",
      best.method = "youden",
      best.weights = best_weights,
      transpose = FALSE
    ) |>
    dplyr::mutate(.is_best_youden = TRUE)

  closest <- x |>
    pROC::coords(
      ret = c("specificity", "sensitivity"),
      x = "best",
      best.method = "closest.topleft",
      best.weights = best_weights,
      transpose = FALSE
    ) |>
    dplyr::mutate(.is_best_closest_topleft = TRUE)

  rename_plan <- c(
    .sensitivities = "sensitivity",
    .specificities = "specificity"
  )

  coords |>
    dplyr::left_join(
      dplyr::distinct(youden),
      c("specificity", "sensitivity"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      dplyr::distinct(closest),
      c("specificity", "sensitivity"),
      relationship = "many-to-one"
    ) |>
    dplyr::filter(.data$.is_best_closest_topleft | .data$.is_best_youden) |>
    dplyr::mutate(
      .is_best_youden = dplyr::coalesce(.data$.is_best_youden, FALSE),
      .is_best_closest_topleft = dplyr::coalesce(
        .data$.is_best_closest_topleft,
        FALSE
      )
    ) |>
    tibble::as_tibble() |>
    dplyr::rename(tidyselect::all_of(rename_plan))
}


#' Compute AUCs using the trapezoid method
#'
#' @param xs,ys x and y positions
#' @return the area under the curve computed using the trapezoid method. For
#'   `partial_trapezoid_auc()`, the partial area under the curve is computed.
#' @export
#' @examples
#' if (requireNamespace("rstanarm", quietly = TRUE)) {
#'   wells <- rstanarm::wells
#'   r <- pROC::roc(switch ~ arsenic, wells)
#'   pROC::auc(r)
#'   trapezoid_auc(r$specificities, r$sensitivities)
#'
#'   pROC::auc(r, partial.auc = c(.9, 1), partial.auc.focus = "sp")
#'   partial_trapezoid_auc(r$specificities, r$sensitivities, c(.9, 1))
#'
#'   pROC::auc(r, partial.auc = c(.9, 1), partial.auc.focus = "se")
#'   partial_trapezoid_auc(r$sensitivities, r$specificities, c(.9, 1))
#'
#'   pROC::auc(r, partial.auc = c(.1, .9), partial.auc.focus = "sp")
#'   partial_trapezoid_auc(r$specificities, r$sensitivities, c(.1, .9))
#'
#'   pROC::auc(r, partial.auc = c(.1, .9), partial.auc.focus = "se")
#'   partial_trapezoid_auc(r$sensitivities, r$specificities, c(.1, .9))
#' }
#' @concept roc
trapezoid_auc <- function(xs, ys) {
  stopifnot(is_sorted(xs))
  # reorder along increasing x to avoid negative AUC from decreasing xs
  xs_order <- order(xs)
  xs <- xs[xs_order]
  ys <- ys[xs_order]
  widths <- diff(xs)
  ys_steps <- diff(ys) / 2
  heights <- ys[-length(ys)] + ys_steps
  sum(widths * heights)
}


#' @rdname trapezoid_auc
#' @param xlim two-element vector (a range) of the `xs` to sum
#'   over
#' @export
partial_trapezoid_auc <- function(xs, ys, xlim) {
  lower_xlim <- min(xlim)
  upper_xlim <- max(xlim)

  # impute y values on the x limits
  if (! lower_xlim %in% xs) {
    i_results <- impute_point(xs, ys, lower_xlim)
    xs <- i_results$xs
    ys <- i_results$ys
  }

  if (! upper_xlim %in% xs) {
    i_results <- impute_point(xs, ys, upper_xlim)
    xs <- i_results$xs
    ys <- i_results$ys
  }

  xs_r <- xs[lower_xlim <= xs & xs <= upper_xlim]
  ys_r <- ys[lower_xlim <= xs & xs <= upper_xlim]
  trapezoid_auc(xs_r, ys_r)
}


# given c(x1, x_target, x2) and c(y1, y2), impute y_target
impute_point <- function(xs, ys, x_target) {
  splits <- split_vector_indices(xs, x_target)

  x_neighbor <- xs[c(splits$pivot, splits$pivot + 1)]
  y_neighbor <- ys[c(splits$pivot, splits$pivot + 1)]

  # diff(x_neighbor) : total distance
  # abs(x_neighbor - x_target) : distance from each point to target
  # abs() / diff() : proportions of total distance
  # 1 - : flip proportions so that closer point has more influence
  weights <- 1 - (abs(x_neighbor - x_target) / diff(x_neighbor))
  y_target <- stats::weighted.mean(y_neighbor, weights)

  xs <- c(xs[splits$below], x_target, xs[splits$above])
  ys <- c(ys[splits$below], y_target, ys[splits$above])

  list(
    xs = xs,
    ys = ys,
    x_new = x_target,
    y_new = y_target
  )
}

# partition a vector's indices at a key value
split_vector_indices <- function(xs, target) {
  is_decreasing <- !is.unsorted(rev(xs), strictly = FALSE)
  results <- list()
  if (is_decreasing) {
    results$below <- which(target <= xs)
    results$above <- which(xs < target)
  } else {
    results$below <- which(xs <= target)
    results$above <- which(target < xs)
  }
  results$pivot <- results$below[length(results$below)]
  results
}

is_sorted <- function(xs) {
  pos_unsorted <- is.unsorted( xs, strictly = FALSE)
  neg_unsorted <- is.unsorted(-xs, strictly = FALSE)
  !pos_unsorted || !neg_unsorted
}

is_increasing <- function(xs) {
  signs <- sign(diff(xs))
  is_not_strictly_decreasing <- all(0 <= signs)
  is_increasing_somewhere <- any(signs == 1)
  is_not_strictly_decreasing && is_increasing_somewhere
}

is_decreasing <- function(xs) {
  is_increasing(rev(xs))
}
