
#' Impute values

# build_wider_spec_for_imputation(fake_data, x, level)

# spec <- build_wider_spec_for_imputation(fake_data, x, level)






# rlang::new_formula()

# pivot_wider_for_imputation <- function(data, spec, id_cols = NULL) {
#   data |>
#     tidyr::pivot_wider_spec(spec, id_cols = id_cols)
# }



# We can get CP imputations from the TD by using `data_train`
impute_values_by_length <- function(data, var_y, var_length, id_cols = NULL, data_train = NULL) {

  fake_data <- tibble::tibble(
    child = c(
      "a", "a", "a", "a", "a",
      "b", "b", "b", "b", "b",
      "c", "c", "c", "c", "c",
      "e", "e", "e", "e", "e",
      "f", "f", "f", "f", "f",
      "g", "g", "g", "g", "g",
      "h", "h", "h", "h", "h"
    ),
    level = c(1:5, 1:5, 1:5, 1:5, 1:5, 1:5, 1:5),
    x = c(
      100, 110, 120, 130, 150,
      80,  91, 100, 112, NA,
      82,  95, 103, 110, 127,
      70,  77, 85,   NA, NA,
      102, 115, 123,  NA, NA,
      c(100, 110, 120, 130, 150) + c(1, 2, 3, 1, 0),
      c(100, 110, 120, 130, 150) + c(-1, -3, 1, 3, 3)
    )
  )
  data <- fake_data

  build_wider_spec_for_imputation <- function(data, var_y, var_length) {
    tidyr::build_wider_spec(
      data,
      names_from = {{ var_length }},
      names_prefix = "y_",
      values_from = {{ var_y }}
    )
  }

  get_position_of_last_non_na_value <- function(xs) {
    max(which(!is.na(xs)))
  }

  fit_imputation_models <- function(data, spec) {
    create_formulas <- function(spec) {
      n_models <- nrow(spec)
      l <- seq_len(n_models) |> as.list()
      names <- spec[, ".name", drop = TRUE]

      for (i in seq_len(n_models)) {
        # force(i)
        if (i == 1) next()
        # the last item does not need the length of longest variable
        # as it is constant here
        ll_var <- if (i != n_models) ".max_var_length" else character(0)

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

    l <- create_formulas(spec)
    lapply(l, lm, data = data)
  }



  var <- quote(x)
  var_length <- quote(level)
  id_cols = quote(child)

  spec <- build_wider_spec_for_imputation(data, {{ var }}, {{ var_length }})

  data_wide <- tidyr::pivot_wider_spec(data, spec, id_cols = id_cols)

  data_wide[[".max_var_length"]] <- data_wide |>
    dplyr::select(-1) |>
    apply(1, last_non_na)


  if (is.null(data_train)) {
    data_train <- data
  }

  data_wide_train <- tidyr::pivot_wider_spec(data_train, spec, id_cols = id_cols)
  data_wide_train[[".max_var_length"]] <- data_wide_train |>
    dplyr::select(-1) |>
    apply(1, last_non_na)

  models <- fit_imputation_models(data_wide_train, spec)

  data_imputed <- data_wide
  for (y_name in names(models)) {
    vals <- data_imputed[[y_name]]
    data_imputed[[y_name]] <- ifelse(
      is.na(vals),
      predict(models[[y_name]], data_imputed),
      vals
    )

  }


  data_imputed_long <- data_imputed |>
    tidyr::pivot_longer_spec(spec) |>
    dplyr::rename("imputed_{{ var }}" := {{ var }})

  data_original_values <- data_wide |>
    tidyr::pivot_longer_spec(spec)

  join_set <- data_imputed_long |>
    dplyr::select(id_cols, {{ var_length }}, .max_var_length)

  data_original_values |>
    dplyr::left_join(data_imputed_long, colnames(join_set))

  # data_original_values |>
  #   left_join(data_imputed_long)
  #
  # data_imputed_long
  # d <- data_imputed |>
  #   tidyr::pivot_longer_spec(spec) |>
  #   rename("imputed_{{ var }}" := {{ var }}) |>
  #   select(-length_longest) |>
  #   left_join(
  #     data_child_ll,
  #     by = c("group", "subject_num", "visit_id", "age_months")
  #   ) |>
  #   left_join(
  #     data_original_values,
  #     by = c("group", "subject_num", "visit_id", "tocs_level")
  #   ) |>
  #   mutate(
  #     imputed = ifelse(is.na({{ var }}), "imputed", "observed"),
  #     facet_lab = paste0(tocs_level, " words")
  #   )

  # d
}
