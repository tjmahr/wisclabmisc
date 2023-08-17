
#' Staged imputation
#'
#' Impute missing data at different utterance lengths using successive linear
#' models.
#'
#' @param data dataframe in which to impute missing value
#' @param var_y bare name of the response variable for imputation
#' @param var_length bare name of the length variable
#' @param id_cols a selection of variable names that uniquely identify each
#'   group of related observations. For example, `c(child_id, age_months)`.
#' @param include_max_length whether to use the maximum length value as a
#'   predictor in the imputation models. Defaults to `FALSE`.
#' @param data_train (optional) dataframe used to train the imputation models.
#'   For example, we might have data from a reference group of children in
#'   `data_train` but a clinical population in `data`. If omitted, the dataframe
#'   in `data` is used to train the models.
#' @return a dataframe with the additional columns `{var_y}_imputed` (the
#'   imputed value), `.max_{var_length}` with the highest value of `var_length`
#'   with observed data, and `{var_y}_imputation` for labeling whether
#'   observations were `"imputed"` or `"observed"`.
#' @export
#' @details
#'
#' # Background
#'
#' In Hustad and colleagues (2020), we modeled intelligibility data in young
#' children's speech. Children would hear an utterance and then they would
#' repeat it. The utterances started at 2 words in length, then increased to 3
#' words in length, and so on in batches of 10 sentences, all the way to 7
#' words in length. There was a problem, however: Not all of the children could
#' produce utterances at every length. Specifically, if a child could not
#' reliably produced 5 utterances of a given length length, the task was halted.
#' So given the nature of the task, if a child had produced 5-word utterances,
#' they also produced 2--4-word utterances as well.
#'
#' The length of the utterance probably influenced the outcome variable: Longer
#' utterances have more words that might help a listener understand the
#' sentence, for example. Therefore, it did not seem appropriate to ignore the
#' missing values. We used the following two-step procedure (see the
#' [Supplemental Materials](https://doi.org/10.23641/asha.12330956.v1) for more
#' detail):
#'
#'
#' ## Other notes
#'
#' Remark about `data` and `data_train`: One might ask, *why shouldn't some
#' children help train the data imputation models?* Let's consider a
#' norm-referenced standardized testing scenario: We have a new participant
#' (observations in `data`), and we want to know how they compare to their age
#' peers (participants in `data_train`). By separating out `data_train` and
#' fixing it to a reference group, we can apply the same adjustment/imputation
#' procedure to all new participants.
#'
#' @references Hustad, K. C., Mahr, T., Natzke, P. E. M., & Rathouz, P. J.
#'   (2020). Development of Speech Intelligibility Between 30 and 47 Months in
#'   Typically Developing Children: A Cross-Sectional Study of Growth. *Journal
#'   of Speech, Language, and Hearing Research*, *63*(6), 1675–1687.
#'   https://doi.org/10.1044/2020_JSLHR-20-00008
#'
#'   Hustad, K. C., Mahr, T., Natzke, P. E. M., & J. Rathouz, P. (2020).
#'   Supplemental Material S1 (Hustad et al., 2020). ASHA journals.
#'   https://doi.org/10.23641/asha.12330956.v1
#'
#' @examples
#' fake_data <- tibble::tibble(
#'   child = c(
#'     "a", "a", "a", "a", "a",
#'     "b", "b", "b", "b", "b",
#'     "c", "c", "c", "c", "c",
#'     "e", "e", "e", "e", "e",
#'     "f", "f", "f", "f", "f",
#'     "g", "g", "g", "g", "g",
#'     "h", "h", "h", "h", "h",
#'     "i", "i", "i", "i", "i"
#'   ),
#'   level = c(1:5, 1:5, 1:5, 1:5, 1:5, 1:5, 1:5, 1:5),
#'   x = c(
#'     c(100, 110, 120, 130, 150) + c(-8, -5, 0, NA, NA),
#'     c(100, 110, 120, 130, 150) + c(6, 6, 4, NA, NA),
#'     c(100, 110, 120, 130, 150) + c(-5, -5, -2, 2, NA),
#'     c(100, 110, 120, 130, 150) + rbinom(5, 12, .5) - 6,
#'     c(100, 110, 120, 130, 150) + rbinom(5, 12, .5) - 6,
#'     c(100, 110, 120, 130, 150) + rbinom(5, 12, .5) - 6,
#'     c(100, 110, 120, 130, 150) + rbinom(5, 12, .5) - 6,
#'     c(100, 110, 120, 130, 150) + rbinom(5, 12, .5) - 6
#'   )
#' )
#' data_imputed <- impute_values_by_length(fake_data, x, level, id_cols = c(child))
#'
#' if (requireNamespace("ggplot2")) {
#'   library(ggplot2)
#'   ggplot(data_imputed) +
#'     aes(x = level, y = x_imputed) +
#'     geom_line(aes(group = child)) +
#'     geom_point(aes(color = x_imputation))
#' }
impute_values_by_length <- function(
    data,
    var_y,
    var_length,
    id_cols = NULL,
    include_max_length = FALSE,
    data_train = NULL
) {

  get_position_of_last_non_na_value <- function(xs) {
    max(which(!is.na(xs)))
  }

  fit_imputation_models <- function(data, spec, pred_var_max_length) {
    create_formulas <- function(spec, pred_var_max_length) {
      n_models <- nrow(spec)
      l <- seq_len(n_models) |> as.list()
      names <- spec[, ".name", drop = TRUE]

      for (i in seq_len(n_models)) {
        if (i == 1) next()
        # the last item does not need the length of longest variable
        # as it is constant here
        ll_var <- if (i != n_models) pred_var_max_length else character(0)

        lhs_var <- names[i]
        rhs_vars <- names[seq(1, i - 1)] |>
          c(ll_var) |>
          paste(collapse = " + ")

        l[[i]] <- rlang::new_formula(
          lhs = rlang::sym(lhs_var),
          rhs = rlang::parse_expr(rhs_vars)
        )
      }
      names(l) <- names
      l[[1]] <- NULL
      l
    }

    create_formulas(spec, pred_var_max_length) |>
      lapply(stats::lm, data = data)
  }

  chr_var_y <- rlang::englue("{{ var_y }}")
  chr_var_length <- rlang::englue("{{ var_length }}")
  chr_var_max_length <- rlang::englue(".max_{{ var_length }}")
  chr_var_y_imputed <- rlang::englue("{{ var_y }}_imputed")
  chr_var_y_imputation <- rlang::englue("{{ var_y }}_imputation")

  # Create a wide dataframe with columns y_1, y_2, y_3, etc. so that
  # we can do models likes y_3 ~ y_2 + y_1.
  spec <- tidyr::build_wider_spec(
    data,
    names_from = {{ var_length }},
    names_prefix = "y_",
    values_from = {{ var_y }}
  )

  data_wide <- tidyr::pivot_wider_spec(data, spec, id_cols = {{ id_cols }})

  # Store the longest length value without missing data
  data_wide[[chr_var_max_length]] <- data_wide |>
    dplyr::select(dplyr::all_of(spec$.name)) |>
    apply(1, get_position_of_last_non_na_value)

  # Set up the dataset for training the models if they are provided
  if (is.null(data_train)) {
    data_train <- data
    data_wide_train <- data_wide
  } else {
    data_wide_train <- tidyr::pivot_wider_spec(
      data_train,
      spec = spec,
      id_cols = {{ id_cols }}
    )
    data_wide_train[[chr_var_max_length]] <- data_wide_train |>
      dplyr::select(dplyr::all_of(spec$.name)) |>
      apply(1, get_position_of_last_non_na_value)
  }

  # Train the imputation model
  if (!include_max_length) {
    pred_var_max_length <- character(0)
  } else {
    pred_var_max_length <- chr_var_max_length
  }
  models <- fit_imputation_models(data_wide_train, spec, pred_var_max_length)

  # Impute. Loop so that imputed y_i can be used as predictor for a later y_j
  data_imputed <- data_wide
  for (y_name in names(models)) {
    vals <- data_imputed[[y_name]]
    data_imputed[[y_name]] <- ifelse(
      is.na(vals),
      stats::predict(models[[y_name]], data_imputed),
      vals
    )
  }

  # Reshape back to long.
  data_imputed_long <- data_imputed |>
    tidyr::pivot_longer_spec(spec) |>
    dplyr::rename(!! chr_var_y_imputed := {{ var_y }})

  data_original_values <- data_wide |>
    tidyr::pivot_longer_spec(spec)

  join_set <- data_imputed_long |>
    dplyr::select(
      {{ id_cols }},
      tidyselect::all_of(c(chr_var_length, chr_var_max_length))
    ) |>
    colnames()

  data_both <- data_original_values |>
    dplyr::left_join(data_imputed_long, join_set)

  data_both[[chr_var_y_imputation]] <- ifelse(
    is.na(data_both[[chr_var_y]]) & !is.na(data_both[[chr_var_y_imputed]]),
    "imputed",
    "observed"
  )

  data_both
}
