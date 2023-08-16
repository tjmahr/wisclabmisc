

#' Staged imputation for multiword utterances
#'
#' @keywords internal
#' @rdname imputation
build_wider_spec_for_imputation <- function(data, var, var_stage) {
  tidyr::build_wider_spec(
    data,
    # {{ }}: plugs in variable name
    names_from = {{ var_stage }},
    names_prefix = "y_",
    values_from = {{ var }}
  )
}

pivot_wider_for_imputation <- function(data, spec, id_cols = NULL) {
  if (is.null())
  data %>%
    tidyr::pivot_wider_spec(
      spec,
      id_cols = c(
        group, subject_num, visit_id, length_longest, age_months, tocs_level
      )
    )
}

fit_imputation_models <- function(data) {
  list(
    m_7 = lm(y_7 ~ y_1 + y_2 + y_3 + y_4 + y_5 + y_6, data),
    m_6 = lm(y_6 ~ y_1 + y_2 + y_3 + y_4 + y_5 + length_longest, data),
    m_5 = lm(y_5 ~ y_1 + y_2 + y_3 + y_4 + length_longest, data),
    m_4 = lm(y_4 ~ y_1 + y_2 + y_3 + length_longest, data),
    m_3 = lm(y_3 ~ y_1 + y_2 + length_longest, data)
  )
}


# We can get CP imputations from the TD by using `data_train`
impute_values <- function(data, var, data_train = NULL) {
  spec <- build_wider_spec_for_imputation(data, {{ var }})
  data_wide <- pivot_wider_for_imputation(data, spec)

  if (is.null(data_train)) {
    data_train <- data
  }

  data_wide_train <- pivot_wider_for_imputation(data_train, spec)
  models <- fit_imputation_models(data_wide_train)

  data_imputed <- data_wide %>%
    mutate(
      y_3 = ifelse(is.na(y_3), predict(models$m_3, .), y_3)
    ) %>%
    mutate(
      y_4 = ifelse(is.na(y_4), predict(models$m_4, .), y_4)
    ) %>%
    mutate(
      y_5 = ifelse(is.na(y_5), predict(models$m_5, .), y_5)
    ) %>%
    mutate(
      y_6 = ifelse(is.na(y_6), predict(models$m_6, .), y_6)
    ) %>%
    mutate(
      y_7 = ifelse(is.na(y_7), predict(models$m_7, .), y_7)
    )

  # handle these separately because `length_longest` could have been imputed if
  # there was a different strategy
  data_child_ll <- data_wide %>%
    distinct(group, subject_num, visit_id, age_months, length_longest)

  data_original_values <- data_wide %>%
    tidyr::pivot_longer_spec(spec) %>%
    distinct(group, subject_num, visit_id, tocs_level, {{ var }})

  d <- data_imputed %>%
    tidyr::pivot_longer_spec(spec) %>%
    rename("imputed_{{ var }}" := {{ var }}) %>%
    select(-length_longest) %>%
    left_join(
      data_child_ll,
      by = c("group", "subject_num", "visit_id", "age_months")
    ) %>%
    left_join(
      data_original_values,
      by = c("group", "subject_num", "visit_id", "tocs_level")
    ) %>%
    mutate(
      imputed = ifelse(is.na({{ var }}), "imputed", "observed"),
      facet_lab = paste0(tocs_level, " words")
    )

  d
}

