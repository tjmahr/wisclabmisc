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

