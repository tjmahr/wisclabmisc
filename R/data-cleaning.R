
#' Extract the TOCS details from a string (usually a filename)
#' @param xs a character vector
#' @return `tocs_item()` returns the substring with the TOCS item, `tocs_type()`
#'   returns whether the item is `"single-word"` or `"multiword"`, and
#'   `tocs_length()` returns the length of the TOCS item (i.e., the number of
#'   words).
#' @rdname tocs_item
#' @concept data-cleaning
#' @export
#' @examples
#' x <- c(
#'   "XXv16s7T06.lab", "XXv15s5T06.TextGrid", "XXv13s3T10.WAV",
#'   "XXv18wT11.wav", "non-matching", "s2T01"
#' )
#' data.frame(
#'   x = x,
#'   item = tocs_item(x),
#'   type = tocs_type(x),
#'   length = tocs_length(x)
#' )
tocs_item <- function(xs) {
  xs |>
    toupper() |>
    stringr::str_extract(
      "(S[2-7]|W)(T)[0-4][0-9](?=([.]WAV|[.]TEXTGRID|[.]LAB|$))"
    )
}

#' @rdname tocs_item
#' @export
tocs_type <- function(xs) {
  starts <- xs |> tocs_item() |> substr(1, 1)
  types <- rep_len(NA_character_, length(starts))
  types[starts == "W"] <- "single-word"
  types[starts == "S"] <- "multiword"
  types
}

#' @rdname tocs_item
#' @export
tocs_length <- function(xs) {
  items <- xs |> tocs_item()
  char2 <- substr(items, 2, 2)
  char2[char2 %in% "T"] <- "1"
  as.integer(char2)
}
