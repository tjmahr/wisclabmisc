

#' Create an ROC curve from smoothed densities
#'
#' @param data a dataframe containing densities
#' @param controls,cases bare column names for the densities of the control
#'   group and case group
#' @param along optional bare column name for the response values
#' @param best_weights weights for computing the best ROC curve points. Defaults
#'   to `c(1, .5)`, which are the defaults used by `pROC::coords()`.
#' @param direction `direction` for computing case status. `pROC::roc()`'s
#'   `direction` conventions are supported: `"<"` (i.e., `control < case`) and
#'   `">"` (`control > case`), or `"auto"` to have pROC guess the direction (if
#'   applicable). Alternatively, more verbose directions are supported:
#'   `"case-low"` or `"control-high"` if a low score predicts case status
#'   (`case <= threshold < control`, analogous to pROC's `">"`) and `"case-high"`
#'   or `"control-low"` if a high score predicts case status
#'   (`control < threshold <= case`, analogous to pROC's `"<"`). These
#'   directions are translated into the pROC conventions.
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
    direction = "auto"
) {
  direction <- validate_roc_direction(direction, TRUE)

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
#'   variable
#' @param response a bare column name with the group status (control vs. cases).
#'   If the response has more than two groups, the first element of `levels` is
#'   the control group and the second element of `levels` is the case group.
#' @param predictor a bare column name with the predictor to use for
#'   classification
#' @param levels two-element vector `c(control, case)` where `control` is the
#'   value of `response` for the control group and `case` is the value of
#'   `response` for the case group. The ordering matters: The first element of
#'   the vector names the control group.
#' @inheritParams compute_smooth_density_roc
#' @param ... additional arguments passed to `pROC::roc()`.
#' @return a new dataframe of ROC coordinates is returned with columns for the
#'   predictor variable, `.sensitivities`, `.specificities`, `.auc`,
#'   `.direction`, `.controls`, `.cases`, `.n_controls`, `.n_cases`,
#'   `.is_best_youden` and `.is_best_closest_topleft`.
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
    levels = NULL,
    ...
) {
  q_response <- enquo(response)
  q_predictor <- enquo(predictor)
  x <- select(data, !! q_predictor)
  y <- select(data, !! q_response)
  level_info <- validate_roc_levels(y, levels)
  levels <- level_info$levels
  direction <- validate_roc_direction(direction, TRUE)

  roc <- pROC::roc_(
    data,
    response = colnames(y)[1],
    predictor = colnames(x)[1],
    direction = direction,
    levels = levels,
    ...
  )

  tidied <- tidy_best_roc_coords(roc, best_weights = best_weights)

  rename_plan <- c(
    .sensitivities = "sensitivity",
    .specificities = "specificity"
  )

  rename_plan_2 <- c("threshold") |>
    stats::setNames(names(x)[1])

  d <-pROC::coords(roc) |>
    dplyr::rename(tidyselect::all_of(rename_plan)) |>
    dplyr::mutate(
      .auc = as.numeric(roc$auc),
      .direction = roc$direction,
      .controls = roc$levels[1],
      .cases = roc$levels[2],
      .n_controls = sum(y[[1]] == roc$levels[1]),
      .n_cases = sum(y[[1]] == roc$levels[2])
    ) |>
    dplyr::left_join(
      dplyr::distinct(tidied),
      by = c(".sensitivities", ".specificities", "threshold"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      .is_best_youden = dplyr::coalesce(.data$.is_best_youden, FALSE),
      .is_best_closest_topleft = dplyr::coalesce(
        .data$.is_best_closest_topleft,
        FALSE
      )
    ) |>
    dplyr::rename(tidyselect::all_of(rename_plan_2)) |>
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


#' Compute sensitivity and specificity scores from (weighted) observed data
#'
#' `pROC::roc()` does not support observation-weights when computing ROC curves.
#' This function fills that gap.
#'
#' @inheritParams compute_empirical_roc
#' @param weights a bare column name for the observation weights. If `weights`
#'   is set to `NULL` or not provided, all values receive equal weight and
#'   conventional sensitivity and specificity scores are returned.
#' @return A dataframe of stepwise empirical ROC coordinates computed from
#'   (optionally weighted) data. The output includes columns for the predictor
#'   variable, `.sensitivities`, `.specificities`, `.auc`, `.comparison`,
#'   `.n_controls`, `.n_cases`, `.w_controls`, `.w_cases`, `.comparison`,
#'   `.direction`, `.response`, `.controls`, and `.cases`. `n_` columns contain
#'   the number of observations for that predictor value and `w_` contain the
#'   total weight of the observations for that predictor value.
#'
#' @details
#' The `.sensitivities` and `.specificities` columns are calculated directly
#' from the (weighted) ECDFs of the controls and cases, so no call to
#' `pROC::roc()` is made.
#'
#' `.auc` is calculated using `trapezoid_auc()`.
#'
#' `c(-Inf, Inf)` are added to the `predictor` vector so that sensitivities and
#' specificities range from 0 to 1.
#'
#' @concept roc
#' @export
#' @examples
#' # Simulate 3-class dataset
#' set.seed(100)
#' n <- 50
#' means <- c("A" = 0, "B" = 1, "C" = 2)
#' y <- sample(c("A", "B", "C"), n, replace = TRUE)
#' x <- rnorm(n, mean = means[y], sd = 1)
#' w <- runif(n, 0.5, 2)
#' df <- data.frame(y, x, w)
#'
#' # Compare "A" (controls) to "C" (cases)
#' roc_tbl <- compute_sens_spec_from_ecdf(
#'   data = df,
#'   response = y,
#'   predictor = x,
#'   weights = w,
#'   direction = "control-low",
#'   levels = c("A", "C")
#' )
#' dplyr::glimpse(roc_tbl)
#'
#' # Compare "B" (controls) to "C" (cases)
#' roc_tbl2 <- compute_sens_spec_from_ecdf(
#'   data = df,
#'   response = y,
#'   predictor = x,
#'   weights = w,
#'   direction = "control-low",
#'   levels = c("B", "C")
#' )
#' dplyr::glimpse(roc_tbl2)
compute_sens_spec_from_ecdf <- function(
    data,
    response,
    predictor,
    weights = NULL,
    direction = NULL,
    levels = NULL
) {
  q_response <- enquo(response)
  q_predictor <- enquo(predictor)
  q_weights <- enquo(weights)
  x <- dplyr::select(data, !! q_predictor)
  y <- dplyr::select(data, !! q_response)
  direction <- validate_roc_direction(direction, auto_allowed = FALSE)
  level_info <- validate_roc_levels(y, levels)
  levels <- level_info$levels

  d1 <- data[level_info$l1_rows, ]
  d2 <- data[level_info$l2_rows, ]

  x1 <- d1 |> dplyr::pull(!! q_predictor)
  x2 <- d2 |> dplyr::pull(!! q_predictor)

  if (!rlang::quo_is_null(q_weights)) {
    w1 <- d1 |> dplyr::pull(!! q_weights)
    w2 <- d2 |> dplyr::pull(!! q_weights)
  } else {
    w1 <- rep(1, length(x1))
    w2 <- rep(1, length(x2))
  }

  data_out <- .compute_ecdf_sens_spec(x1, w1, x2, w2, direction)
  names(data_out)[names(data_out) == "x"] <- colnames(x)[1]
  data_out$.direction <- direction
  data_out$.response <- colnames(y)[1]
  data_out$.controls <- levels[1]
  data_out$.cases <- levels[2]
  data_out$.auc <- trapezoid_auc(
    data_out$.specificities,
    data_out$.sensitivities
  )

  data_out$.comparison <- sprintf(
    "ctrl (%s %s) %s case (%s %s)",
    colnames(y)[1], levels[1],
    direction,
    colnames(y)[1], levels[2]
  )

  col_order <- c(
    colnames(x)[1],
    ".sensitivities",
    ".specificities",
    ".auc",
    ".comparison",
    ".n_controls",
    ".n_cases",
    ".w_controls",
    ".w_cases",
    ".direction",
    ".response",
    ".controls",
    ".cases",
    ".is_best_youden",
    ".is_best_closest_topleft"
  )

  data_out[intersect(col_order, names(data_out))]
}


.compute_ecdf_sens_spec <- function(x1, w1, x2, w2, direction) {
  d <- data.frame(
    x = c(x1, x2, -Inf, Inf),
    w = c(w1, w2, 0, 0),
    in_x1 = c(rep(1, length(x1)), rep(0, length(x2)), 0, 0),
    in_x2 = c(rep(0, length(x1)), rep(1, length(x2)), 0, 0)
  )
  d1 <- d |>
    dplyr::arrange(.data$x) |>
    dplyr::group_by(.data$x) |>
    dplyr::summarise(
      .n_controls = sum(.data$in_x1),
      .n_cases = sum(.data$in_x2),
      .w_controls = sum(.data$in_x1 * .data$w),
      .w_cases = sum(.data$in_x2 * .data$w),
    ) |>
    dplyr::arrange(.data$x) |>
    dplyr::mutate(
      .sensitivities = cumsum(.data$.w_cases) / sum(.data$.w_cases),
      .specificities = 1 - (cumsum(.data$.w_controls) / sum(.data$.w_controls))
    )

  if (direction == "<") {
    d1$.sensitivities <- 1 - d1$.sensitivities
    d1$.specificities <- 1 - d1$.specificities
  }

  d1
}


validate_roc_levels <- function(data_y, levels = NULL) {
  y_name <- colnames(data_y)[1]
  y_vals <- data_y[, 1, drop = TRUE]

  if (is.null(levels)) {
    obs <- y_vals |> as.factor() |> levels()
    levels <- obs[1:2]
    plevels <- levels |> encodeString(quote = "\"")
    clevels <- plevels |> paste0(collapse = ", ")
    cli::cli_inform(c(
      "i" = "No {.arg levels} provided. Using {.code levels = c({clevels})}.",
      "*" = "Setting {.emph control} to {.field {y_name}} == {plevels[1]}.",
      "*" = "Setting {.emph case} to {.field {y_name}} == {plevels[2]}."
    ))
  }

  if (length(unique(levels)) != 2) {
    plevels <- levels |> base::encodeString(quote = '"')
    clevels <- plevels |> paste0(collapse = ", ")
    cli::cli_abort(c("Provided {.arg levels} {.code c({clevels})} must have 2 unique values."))
  }

  are_levels_in_y <- all(levels %in% y_vals)
  if (!are_levels_in_y) {
    plevels <- levels |> base::encodeString(quote = '"')
    clevels <- plevels |> paste0(collapse = ", ")
    cli::cli_abort(c("Provided {.arg levels} {.code c({clevels})} must be values of {.arg response} ({.field {y_name}})"))
  }

  list(
    levels = levels,
    l1_rows = which(y_vals == levels[1]),
    l2_rows = which(y_vals == levels[2])
  )
}

validate_roc_direction <- function(direction, auto_allowed) {
  case_low <- c(">", "case-low", "control-high")
  case_high <- c("<", "case-high", "control-low")
  auto <- if (auto_allowed) c("auto") else character(0)
  valid_directions <- c(case_low, case_high, auto)
  bad_direction <- !is.element(direction, valid_directions)

  if (is.null(direction) || isTRUE(bad_direction)) {
    style_or <- list("vec-last" = ", or ", "vec-sep2" = " or ")
    ch <- cli::cli_vec(case_high, style = style_or)
    cl <- cli::cli_vec(case_low, style = style_or)
    au <- cli::cli_vec(auto, style = style_or)
    bullet3 <- if (auto_allowed) {
      c("*" = "To guess the direction, use {.val {auto}}")
    } else {
      character(0)
    }
    cli::cli_abort(c(
      "Provide a valid {.arg direction}",
      "*" = "For control < case, use {.val {ch}}",
      "*" = "For control > case, use {.val {cl}}",
      bullet3
    ))
  }

  if (direction %in% case_low) {
    direction <- case_low[1]
  } else if (direction %in% case_high) {
    direction <- case_high[1]
  }

  direction
}


#' Compute AUCs using the trapezoid method
#'
#' @param xs,ys x and y positions
#' @return the area under the curve computed using the trapezoid method. For
#'   `partial_trapezoid_auc()`, the partial area under the curve is computed.
#' @export
#' @examples
#' # Predict whether a car has automatic vs. manual transmission from mpg
#' r <- pROC::roc(am ~ mpg, mtcars)
#' pROC::auc(r)
#' trapezoid_auc(r$specificities, r$sensitivities)
#'
#' pROC::auc(r, partial.auc = c(.9, 1), partial.auc.focus = "sp")
#' partial_trapezoid_auc(r$specificities, r$sensitivities, c(.9, 1))
#'
#' pROC::auc(r, partial.auc = c(.9, 1), partial.auc.focus = "se")
#' partial_trapezoid_auc(r$sensitivities, r$specificities, c(.9, 1))
#'
#' pROC::auc(r, partial.auc = c(.1, .9), partial.auc.focus = "sp")
#' partial_trapezoid_auc(r$specificities, r$sensitivities, c(.1, .9))
#'
#' pROC::auc(r, partial.auc = c(.1, .9), partial.auc.focus = "se")
#' partial_trapezoid_auc(r$sensitivities, r$specificities, c(.1, .9))
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
  isTRUE(is_not_strictly_decreasing) && isTRUE(is_increasing_somewhere)
}

is_decreasing <- function(xs) {
  is_increasing(rev(xs))
}
