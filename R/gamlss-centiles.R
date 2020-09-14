
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
#' @param newdata a one-column dataframe for predictuions
#' @param model a gamlss model
#' @param data data used to train the model
#' @param centiles centiles to use for prediction. Defaults to `c(5, 10, 50, 90,
#'   95)`.
#' @param ... arguments passed to `gamlss::centiles.pred()`
#' @param data centile predictions to reshape for `pivot_centiles_longer()`
#' @return a tibble with fitted centiles for `predict_centiles()` and a
#'   long-format tibble with one centile value per row in
#'   `pivot_centiles_longer()`
#' @export
#' @rdname predict_centiles
#' @examples
#'
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
    rlang::set_names(stringr::str_replace, "^C?(\\d{1,2})$", "q\\1")

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
      cols = matches("^q\\d+$"),
      names_prefix = "q",
      names_to = ".quantile",
      values_to = ".value"
    )

  has_1090 <- has_pair(long_data$.quantile, "10", "90")
  has_0595 <- has_pair(long_data$.quantile,  "5", "95")
  has_2575 <- has_pair(long_data$.quantile, "25", "75")

  long_data %>%
    dplyr::mutate(
      .pair = dplyr::case_when(
        .quantile %in% c("50") ~ "median",
        .quantile %in% c("10", "90") & has_1090 ~ "quantiles 10, 90",
        .quantile %in% c( "5", "95") & has_0595 ~ "quantiles 05, 95",
        .quantile %in% c("25", "75") & has_2575 ~ "quantiles 25, 75",
      )
    )
}

check_sample_centiles <- function(data, model, var_x, var_y, centiles = c(5, 10, 25, 50, 75, 90, 95)) {
    quo_x <- enquo(var_x)
    quo_y <- enquo(var_y)

    d_quantiles <- data %>%
      dplyr::select(!! quo_x) %>%
      predict_centiles(
        model = model,
        centiles = centiles
      ) %>%
      distinct()

    data %>%
      left_join(d_quantiles, by = rlang::as_name(quo_x)) %>%
      pivot_centiles_longer() %>%
      mutate(
        .quantile = as.numeric(.quantile)
      ) %>%
      group_by(.quantile) %>%
      summarise(
        percent_under_quantile = (mean( (!! quo_y)  <= .value) * 100),
        .groups = "drop"
      ) %>%
      arrange(.quantile)
  }





