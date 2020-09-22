#' Join data onto resampled IDs
#'
#' @param x an rset object created by `rsample::bootstraps()`
#' @param y y dataframe with a column of the id values which was resampled to
#'   create `x`
#' @param by the name of column in y with the data
#' @param validate whether to validate the join by counting the number of rows
#'   associated with each id. Defaults to `FALSE`.
#' @return the original rset object with its `x$data` updated to join with `y`
#'   and with the row numbers `x$in_id` updated to work on the expanded dataset.
#' @export
#' @examples
#' data_trees <- tibble::as_tibble(Orange)
#'
#' data_tree_ids <- distinct(data_trees, Tree)
#'
#' # Resample ids
#' data_bootstraps <- data_tree_ids %>%
#'   rsample::bootstraps(times = 20) %>%
#'   rename(splits_id = splits) %>%
#'   # Attach data to resampled ids
#'   mutate(
#'     data_splits = splits_id %>% purrr::map(
#'       join_to_split,
#'       data_trees,
#'       by = "Tree",
#'       validate = TRUE
#'     )
#'   )
#'
#' data_bootstraps
join_to_split <- function(x, y, by, validate = FALSE) {
  # convert row indices to values in `by`
  old_in_sample_indices <- x$in_id
  old_in_sample_values  <- x$data[[by]][x$in_id]

  old_x <- if (validate) x else NULL

  x$data <- dplyr::left_join(x$data, y, by)

  find_rows <- function(id) {
    which(x$data[[by]] %in% id)
  }

  x$in_id <- purrr::flatten_int(purrr::map(old_in_sample_values, find_rows))

  if (validate) {
    is_valid <- validate_join_to_split(old_x, x, by)
    stopifnot(is_valid)
  }

  x
}

validate_join_to_split <- function(old_x, new_x, by) {
  # create a unique column name by modifying very first one
  temp_name <- sort(names(new_x$data), decreasing = FALSE)[1]
  temp_name <- paste0(".", temp_name)
  new_x$data <- tibble::rowid_to_column(new_x$data, temp_name)

  # count the frequency of ids in old bootstrap
  check1 <- count(rsample::analysis(old_x), .data[[by]])
  # count the frequency of joined rows per id.
  check2 <- count(rsample::analysis(new_x), .data[[by]], .data[[temp_name]])
  check2 <- distinct(check2, .data[[by]], .data[["n"]])

  # The number of times each unique row is resampled for an id should be the
  # same as the number of times the id was reampled.
  all(check1 == check2)
}
