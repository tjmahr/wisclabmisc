#' @export
compute_smooth_density_roc <- function(data, controls, cases, along = NULL) {
  q_controls <- enquo(controls)
  q_cases <- enquo(cases)
  q_along <- enquo(along)

  no_along <- rlang::quo_is_null(q_along)

  old_data <- data

  if (!no_along) {
    data <- arrange(data, !! q_along)
    a <- pull(data, !! q_along)
  }

  x <- pull(data, !! q_controls)
  y <- pull(data, !! q_cases)

  df <- tibble::tibble(
    !! q_controls := c(NA, x, NA),
    !! q_cases    := c(NA, y, NA)
  )

  if (! no_along) {
    df <- mutate(df, !! q_along := c(NA, a, NA))
  }

  roc <- pROC::roc(
    density.controls = x,
    density.cases = y
  )

  join_names <- colnames(df)

  df[[".sensitivities"]] <- roc$sensitivities
  df[[".specificities"]] <- roc$specificities
  df[[".auc"]] <- as.numeric(roc$auc)
  df[[".roc_row"]] <- seq_len(nrow(df))

  old_data %>%
    right_join(df, by = join_names)
}


#' @export
trapezoid_auc <- function(x, y) {
  stopifnot(is_sorted(x))
  # reorder along increasing x to avoid negative AUC from decreasing xs
  x_order <- order(x)
  x <- x[x_order]
  y <- y[x_order]
  widths <- diff(x)
  y_steps <- diff(y) / 2
  heights <- y[-length(y)] + y_steps
  sum(widths * heights)
}


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


# wells <- rstanarm::wells
# r <- pROC::roc(switch ~ arsenic, wells)
#
# pROC::auc(r)
# trapezoid_auc(r$specificities, r$sensitivities)
#
# pROC::auc(r, partial.auc = c(.9, 1), partial.auc.focus = "sp")
# partial_trapezoid_auc(r$specificities, r$sensitivities, c(.9, 1))
#
# pROC::auc(r, partial.auc = c(.9, 1), partial.auc.focus = "se")
# partial_trapezoid_auc(r$sensitivities, r$specificities, c(.9, 1))
#
# pROC::auc(r, partial.auc = c(.1, .9), partial.auc.focus = "sp")
# partial_trapezoid_auc(r$specificities, r$sensitivities, c(.1, .9))
#
# pROC::auc(r, partial.auc = c(.1, .9), partial.auc.focus = "se")
# partial_trapezoid_auc(r$sensitivities, r$specificities, c(.1, .9))
