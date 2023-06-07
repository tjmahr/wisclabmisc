

#' Fit a gamlss model but store user data
#'
#' Think of it as a gamlss model with memories (mem. gamlss).
#'
#' @param ... arguments passed to gamlss::gamlss()
#' @return the fitted `model` object but updated to include user information in
#'   `model$.user`. Includes the dataset used to fit the model
#'   `model$.user$data`, the session info `model$.user$session_info` and the
#'   call used to fit the model `model$.user$call`. `model$call` is updated to
#'   match
#' @export
#' @concept gamlss
mem_gamlss <- function(...) {
  model <- gamlss::gamlss(...)

  model$.user <- list(
    data = eval(model$call[["data"]]),
    session_info = sessioninfo::session_info(),
    call = match.call()
  )

  # spoof the gamlss call so that summary() looks right
  new_call <- model$.user$call
  old_call <- model$call
  new_call[[1]] <- model$call[[1]]
  names(new_call) <- names(old_call)
  model$call <- new_call

  class(model) <- c("mem_gamlss", class(model))
  model
}


#' Predict and tidy centiles from a GAMLSS model
#'
#' gamlss has trouble doing predictions without the original training data.
#'
#' @param newdata a one-column dataframe for predictions
#' @param model a gamlss model prepared by `mem_gamlss()`
#' @param centiles centiles to use for prediction. Defaults to `c(5, 10, 50, 90,
#'   95)`.
#' @param ... arguments passed to `gamlss::centiles.pred()`
#' @param data centile predictions to reshape for `pivot_centiles_longer()`
#' @return a tibble with fitted centiles for `predict_centiles()` and a
#'   long-format tibble with one centile value per row in
#'   `pivot_centiles_longer()`
#' @export
#' @rdname predict_centiles
#' @concept gamlss
predict_centiles <- function(
    newdata,
    model,
    centiles = c(5, 10, 50, 90, 95),
    ...
) {
  if (!inherits(model, "mem_gamlss")) {
    msg <- "
    Refit the model using {usethis::ui_code(\"wisclabmisc::mem_gamlss()\")}
    "
    usethis::ui_stop(msg)
  }

  pred <- model %>%
    gamlss::centiles.pred(
      cent = centiles,
      xname = names(newdata),
      xvalues = newdata[[1]],
      plot = FALSE,
      data = model$.user$data
      # ...
    ) %>%
    rlang::set_names(stringr::str_replace, "^C?(\\d{1,2})$", "c\\1")

  names(pred)[1] <- names(newdata)
  tibble::as_tibble(pred)
}


#' @export
#' @rdname predict_centiles
pivot_centiles_longer <- function(data) {
  has_pair <- function(x, a, b) {
    isTRUE(a %in% x) & isTRUE(b %in% x)
  }

  long_data <- data %>%
    tidyr::pivot_longer(
      cols = matches("^c\\d+$"),
      names_prefix = "c",
      names_to = ".centile",
      values_to = ".value",
      names_transform  = list(.centile = as.numeric),
      values_ptypes = list(.value = numeric())
    )

  has_1090 <- has_pair(long_data$.centile, "10", "90")
  has_0595 <- has_pair(long_data$.centile,  "5", "95")
  has_2575 <- has_pair(long_data$.centile, "25", "75")

  long_data %>%
    mutate(
      .centile_pair = case_when(
        .centile %in% c("50") ~ "median",
        .centile %in% c("10", "90") & has_1090 ~ "centiles 10, 90",
        .centile %in% c( "5", "95") & has_0595 ~ "centiles 05, 95",
        .centile %in% c("25", "75") & has_2575 ~ "centiles 25, 75",
      )
    )
}


#' Compute the percentage of points under each centile line
#'
#' @param data a dataset used to fit a model. If the dataframe is grouped with
#'   `dplyr::group_by()`, sample centiles are computed for each group.
#' @param model a gamlss model prepared by `mem_gamlss()`
#' @param var_x,var_y bare column names of the predictor and outcome variables
#' @param centiles centiles to use for prediction. Defaults to `c(5, 10, 25, 50,
#'   75, 90, 95)`.
#' @return a tibble the number of points and the percentage of points less than
#'   or equal to each quantile value.
#' @export
#' @concept gamlss
check_sample_centiles <- function(
    data,
    model,
    var_x,
    var_y,
    centiles = c(5, 10, 25, 50, 75, 90, 95)
) {
  quo_x <- enquo(var_x)
  quo_y <- enquo(var_y)

  d_quantiles <- data %>%
    ungroup() %>%
    distinct(!! quo_x) %>%
    predict_centiles(
      model = model,
      centiles = centiles
    )

  data %>%
    left_join(d_quantiles, by = rlang::as_name(quo_x)) %>%
    pivot_centiles_longer() %>%
    mutate(.centile = as.numeric(.data$.centile)) %>%
    group_by(.data$.centile, .add = TRUE) %>%
    summarise(
      n = n(),
      n_under_centile = sum(!! quo_y <= .data$.value),
      percent_under_centile = (mean((!! quo_y) <= .data$.value) * 100),
      .groups = "drop_last"
    ) %>%
    ungroup() %>%
    arrange(!!! groups(data), .data$.centile) %>%
    tibble::as_tibble()
}
