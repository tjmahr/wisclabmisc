

#' Convert age in months to years;months
#' @param x a vector ages in months
#' @return ages in the years;months format
#'
#' @details
#' Ages of `NA` return `"NA;NA"`.
#'
#' This format by default is not numerically ordered. This means that `c("2;0",
#' "10;10", "10;9")` would sort as `c("10;10", "10;9", "2;0")`. The function
#' `stringr::str_sort(..., numeric = TRUE)` will sort this vector correctly.
#' @export
#' @rdname ages
#' @examples
#' ages <- c(26, 58, 25, 67, 21, 59, 36, 43, 27, 49)
#' format_year_month_age(ages)
#' @concept etc
format_year_month_age <- function(x) {
  years <- x %/% 12L
  months <- x %% 12L
  paste0(years, ";", months)
}
