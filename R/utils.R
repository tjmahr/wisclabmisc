
#' Convert between ages in months and years;months format
#' @param x
#' * `format_year_month_age()`: a numeric vector of (non-negative) ages in
#'   months.
#' * `parse_year_month_age()`: a character vector of ages in `"years;months"`
#'   format (or `years{sep}months` format more generally).
#' @param sep Separator to use. Defaults to `;`.
#' @return
#' * `format_year_month_age()` returns a character vector in `"years;months"`
#'   format (or `years{sep}months` format more generally).
#' * `parse_year_month_age()` returns an integer vector of ages in months.
#'
#'
#' @details
#' For `format_year_month_age()`, ages of `NA` return `"NA;NA"`.
#'
#' For `parse_year_month_age()`, values that cannot be parsed
#' return `NA`.
#'
#' This format by default is not numerically ordered. This means that `c("2;0",
#' "10;10", "10;9")` would sort as `c("10;10", "10;9", "2;0")`. The function
#' `stringr::str_sort(..., numeric = TRUE)` will sort this vector correctly.
#' @export
#' @rdname ages
#' @examples
#' ages <- c(26, 58, 25, 67, 21, 59, 36, 43, 27, 49, NA)
#' ym_ages <- format_year_month_age(ages)
#' ym_ages
#'
#' parse_year_month_age(ym_ages)
#' @concept data-utils
format_year_month_age <- function(x, sep = ";") {
  stopifnot(length(sep) == 1L)
  assert_whole_number_vector(x, min = 0, allow_missing = TRUE)
  years <- x %/% 12L
  months <- x %% 12L
  paste0(years, sep, months)
}


#' @rdname ages
#' @export
parse_year_month_age <- function(x, sep = ";") {
  stopifnot(length(sep) == 1L)
  # set the context for the error message
  curr_call <- rlang::current_call()

  convert_one <- function(p) {
    # Convert any junk to c(NA, NA)
    if (length(p) != 2L || any(is.na(p))) p <- c(NA_integer_, NA_integer_)
    assert_whole_number_scalar(
      p[1], min = 0, allow_missing = TRUE,
      call = curr_call, arg = "x (years part)"
    )
    assert_whole_number_scalar(
      p[2], min = 0, max = 11, allow_missing = TRUE,
      call = curr_call, arg = "x (months part)"
    )
    12L * p[1] + p[2]
  }

  x |>
    strsplit(sep, fixed = TRUE) |>
    lapply(function(x) suppressWarnings(as.numeric(x))) |>
    vapply(convert_one, numeric(1))
}


#' Compute chronological age in months
#'
#' Ages are rounded down to the nearest month. A difference of 20 months, 29
#' days is interpreted as 20 months.
#'
#' @param t1,t2 dates in "yyyy-mm-dd" format
#' @return the chronological ages in months. NA is returned if the age cannot be
#'   computed.
#' @export
#' @concept data-utils
#' @examples
#' # Two years exactly
#' chrono_age("2014-01-20", "2012-01-20")
#' #> 24
#'
#' # Shift a year
#' chrono_age("2014-01-20", "2013-01-20")
#' #> 12
#' chrono_age("2014-01-20", "2011-01-20")
#' #> 36
#'
#' # Shift a month
#' chrono_age("2014-01-20", "2012-02-20")
#' #> 23
#' chrono_age("2014-01-20", "2011-12-20")
#' #> 25
#'
#' # 3 months exactly
#' chrono_age("2014-05-10", "2014-02-10")
#' #> 3
#'
#' # Borrow a month when the earlier date has a later day
#' chrono_age("2014-05-10", "2014-02-11")
#' #> 2, equal to 2 months, 29 days rounded down to nearest month
#'
#' # Inverted argument order
#' chrono_age("2012-01-20", "2014-01-20")
#' #> 24
#'
#' # Multiple dates
#' t1 <- c("2012-01-20", "2014-02-10", "2010-10-10")
#' t2 <- c("2014-01-20", "2014-05-10", "2014-11-10")
#' chrono_age(t1, t2)
#' #> [1] 24  3 49
chrono_age <- function(t1, t2) {
  stopifnot(length(t1) == length(t2))
  purrr::map2_dbl(t1, t2, purrr::possibly(chrono_age_single, NA))
}

#' Compute difference between two dates in months
#' @noRd
chrono_age_single <- function(t1, t2) {
  difference <- diff_date(t1, t2)
  12 * difference$y  + difference$m
}

#' Compute the difference between two dates
#' @noRd
diff_date <- function(t1, t2) {
  stopifnot(length(t1) == 1, length(t2) == 1)

  if (is.na(t1) || is.na(t2)) {
    warning("Missing date: t1 = ", t1, ", t2 = ", t2, call. = FALSE)
    return(list(y = NA, m = NA, d = NA))
  }

  t1 <- as.Date(t1)
  t2 <- as.Date(t2)

  # Sort dates and convert to a list
  d1 <- as_date_list(min(t1, t2))
  d2 <- as_date_list(max(t1, t2))

  # Borrow a month
  if (d2$d < d1$d) {
    d2$m <- d2$m - 1
    d2$d <- d2$d + 30
  }

  # Borrow a year
  if (d2$m < d1$m) {
    d2$y <- d2$y - 1
    d2$m <- d2$m + 12
  }

  diff <- list(
    y = d2$y - d1$y,
    m = d2$m - d1$m,
    d = d2$d - d1$d
  )
  diff
}

# A lightweight data structure for hand-manipulating dates
as_date_list <- function(date) {
  date <- as.Date(date)
  y <- date |> format("%Y") |> as.numeric()
  m <- date |> format("%m") |> as.numeric()
  d <- date |> format("%d") |> as.numeric()

  list(y = y, m = m, d = d)
}





#' Rename file basenames using functions
#'
#' `file_replace_name()` uses [stringr::str_replace()] to rename files.
#' `file_rename_with()` allows you to rename files with a generic
#' string-transforming function.
#'
#' @export
#' @param path vector of paths for files to rename
#' @param pattern,replacement arguments forwarded to [stringr::str_replace()]
#' @param .fn function to call file paths
#' @param ... arguments passed onto `.fn`
#' @param .dry_run when `FALSE` (the default), files are renamed. When `TRUE`,
#' no files are renamed but the affected files are printed out.
#' @param .overwrite Whether to overwrite files. Defaults to `FALSE` so that
#' overwriting files is opt-in.
#' @return the contents of `paths` with updated file names. Duplicated elements
#' are removed. This function throws an error if a name collision is detected
#' (where two files are both renamed into the same target path).
#' @rdname file_rename_with
#' @details Only the basename of the file (returned by [basename()]
#' undergoes string replacement).
#'
#' @examples
#' # With .dry_run = TRUE, we can make up some file paths.
#' dir <- "//some-fake-location/"
#' path <- file.path(
#'   dir,
#'   c("report_1.csv", "report_2.csv", "report-1.csv", "skipped.csv")
#' )
#'
#' updated <- file_replace_name(path, "report_", "report-", .dry_run = TRUE)
#'
#' # Collisions are detected
#' updated <- file_replace_name(path, "report_\\d", "report-1", .dry_run = TRUE)
file_replace_name <- function(
    path,
    pattern,
    replacement,
    .dry_run = FALSE,
    .overwrite = FALSE
) {
  file_rename_with(
    path = path,
    .fn = stringr::str_replace,
    pattern,
    replacement,
    .dry_run = .dry_run,
    .overwrite = .overwrite
  )
}

#' @rdname file_rename_with
#' @export
file_rename_with <- function(
    path,
    .fn,
    ...,
    .dry_run = FALSE,
    .overwrite = FALSE
) {
  path_old <- fs::path_norm(path)
  basename_old <- basename(path_old)
  basename_new <- .fn(basename_old, ...)
  # basename_new <- stringr::str_replace(basename_old, pattern, replacement)
  path_new <- fs::path(fs::path_dir(path_old), basename_new)

  rename_plan <- analyze_rename_plan(path_old, path_new)

  # Dry run: Show planned changes
  if (.dry_run) {
    cli::cli_inform(prepare_rename_plan_bullets(rename_plan))
    return(invisible(path_old))
  }

  # Collisions always fail
  if (any(rename_plan$has_name_collision)) {
    cli::cli_abort(
      message = c(
        "{sum(rename_plan$has_name_collision)} file{?s} have naming collisions",
        "*" = "Ensure that files do not rename to the same destination",
        "",
        prepare_rename_plan_bullets(rename_plan)
      ),
      call = NULL
    )
  }

  if (any(rename_plan$is_overwrite) & !.overwrite & !.dry_run) {
    cli::cli_abort(
      message = c(
        "{sum(rename_plan$is_overwrite)} file{?s} would be overwritten",
        "*" = "Set {.code .overwrite = TRUE} to deliberately overwrite files",
        "",
        prepare_rename_plan_bullets(rename_plan)
      ),
      call = NULL
    )
  }

  if (any(rename_plan$is_changed)) {
    fs::file_move(
      rename_plan[rename_plan$is_changed, "path_old"],
      rename_plan[rename_plan$is_changed, "path_new"]
    )
  } else {
    cli::cli_inform("No files were renamed.")
  }

  invisible(unique(rename_plan$path_new))
}

analyze_rename_plan <- function(path_old, path_new) {
  is_duplicated <- function(x) duplicated(x) | duplicated(x, fromLast = TRUE)

  is_changed <- path_old != path_new
  is_overwrite <- is_changed & (path_new %in% path_old)
  is_unchanged <- !is_changed & !is_duplicated(path_new)

  # A collision occurs when multiple source files rename to the same target
  is_changed_with_collision <- is_duplicated(path_new[is_changed])
  has_name_collision <- rep(FALSE, length(is_changed))
  has_name_collision[is_changed] <- is_changed_with_collision

  data.frame(
    path_old = path_old,
    path_new = path_new,
    is_changed = is_changed,
    has_name_collision = has_name_collision,
    is_overwrite = is_overwrite,
    is_unchanged = is_unchanged
  )
}


# Prepare bullet points for dry-run output
prepare_rename_plan_bullets <- function(rename_plan) {
  changed <- rename_plan$is_changed
  if (!any(changed)) {
    return(c(
      "Planned changes:",
      " " = "No files would be renamed."
    ))
  }

  changes <- rename_plan |>
    split(~path_new) |>
    lapply(function(df) {
      if (all(df$is_unchanged)) return(character(0))
      old_names <- basename(df[df$is_changed, "path_old"])

      if (length(old_names) > 1) {
        old_part <- paste0(old_names, collapse = ", ") |> sprintf(fmt = "(%s)")
        note <- " {.emph (naming collision)}"
        bullet <- "x"
      } else {
        old_part <- old_names
        note <- ifelse(
          any(df$is_overwrite),
          " {.emph (overwrites an existing file)}",
          ""
        )
        bullet <- ifelse(any(df$is_overwrite), "!",  " ")
      }
      new_part <- unique(basename(df$path_new))
      change <- sprintf("%s -> %s%s", old_part, new_part, note)
      names(change) <- bullet
      change
    }) |>
    unname() |>
    unlist()


  c("Planned changes:", changes)
}






#' Extract the TOCS details from a string (usually a filename)
#' @param xs a character vector
#' @return `tocs_item()` returns the substring with the TOCS item, `tocs_type()`
#'   returns whether the item is `"single-word"` or `"multiword"`, and
#'   `tocs_length()` returns the length of the TOCS item (i.e., the number of
#'   words).
#' @rdname tocs_item
#' @concept data-utils
#' @export
#' @examples
#' x <- c(
#'   "XXv16s7T06.lab", "XXv15s5T06.TextGrid", "XXv13s3T10.WAV",
#'   "XXv18wT11.wav", "non-matching", "s2T01",
#'   "XXv01s4B01.wav", "XXv01wB01.wav",
#'   # sometimes these have tags for *v*irtual visits or recording attempts
#'   "XXv13s3T10v.WAV", "XXv13s3T10a.lab"
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
      "(S[2-7]|W)(T|B)[0-4][0-9](?=[ABCDEV]?([.]WAV|[.]TEXTGRID|[.]LAB|$))"
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
  char2[char2 %in% c("T", "B")] <- "1"
  as.integer(char2)
}




#' Compute overlap rate for (phoneme alignment) intervals
#'
#' @param x1,x2 start and end times for the first interval
#' @param y1,y2 start and end times for the second interval
#' @return the overlap rate
#' @export
#' @details
#' Paulo and Oliveira (2004) provide an "overlap rate" statistic for computing
#' the amount of overlap between two (time) intervals. To my knowledge, nobody
#' has described the Overlap Rate in this way, but it is the
#' [Jaccard index](https://en.wikipedia.org/wiki/Jaccard_index) applied to time
#' intervals.
#'
#' Let \eqn{X=[x_\text{min}, x_\text{max}]} and
#' \eqn{Y=[y_\text{min}, y_\text{max}]} be the sets of times spanned by the
#' intervals \eqn{x} and \eqn{y}. Then, \eqn{X \cap Y} is the *intersection* or the
#' times covered by both intervals, and \eqn{X \cup Y} is the *union* or the
#' times covered by either interval. The size of a set \eqn{A} is denoted
#' \eqn{|A|}. Then the overlap rate is the Jaccard index or the proportion of
#' elements that the two sets have in common:
#'
#' \deqn{\text{overlap rate} = \frac{|X \cap Y|}{|X \cup Y|}}
#'
#' @references Paulo, S., & Oliveira, L. C. (2004). Automatic Phonetic
#' Alignment and Its Confidence Measures. In J. L. Vicedo, P. Martínez-Barco,
#' R. Muńoz, & M. Saiz Noeda (Eds.), *Advances in Natural Language Processing*
#' (pp. 36–44). Springer. <https://doi.org/10.1007/978-3-540-30228-5_4>
#'
#' @examples
#' compute_overlap_rate(
#'   c(0.0, 0.0, 0.0, 0.0),
#'   c(1.0, 1.0, 1.0,  NA),
#'   c(0.5, 2.0, 1.0, 1.0),
#'   c(2.0, 3.0, 2.0, 2.0)
#' )
compute_overlap_rate <- function(x1, x2, y1, y2) {
  lengths <- lengths(list(x1, x2, y1, y2))
  pts <- c(x1, x2, y1, y2)
  stopifnot(
    # they should have length 1 or length N
    all(lengths %in% c(1, max(lengths))),
    # not dealing with negative times for now
    all(pts >= 0 | is.na(pts))
  )

  # normalize intervals so x1 < x2, y1 < y2
  min_x <- pmin(x1, x2)
  max_x <- pmax(x1, x2)
  min_y <- pmin(y1, y2)
  max_y <- pmax(y1, y2)
  dur_x <- max_x - min_x
  dur_y <- max_y - min_y

  # i for "intersect", u for "union"
  min_i <- pmax(min_x, min_y)    # latest start
  max_i <- pmin(max_x, max_y)    # earliest end
  dur_i <- max_i - min_i
  dur_i[dur_i < 0] <- 0          # negative means no overlap
  dur_u <- dur_x + dur_y - dur_i
  dur_u[dur_u == 0] <- NA_real_  # use explicit NA for division by 0
  dur_i / dur_u
}



#' Skip a block of code without executing it
#'
#' `skip_block()` is a lightweight control-flow helper to deliberately
#' skip execution of a block of code while still documenting that the block
#' exists. It is intended as an alternative to commenting out code
#' or wrapping code in `if (FALSE) { ... }`.
#'
#' The function captures the unevaluated code block, reports how many lines
#' would have run, optionally prints a user-supplied message, and then returns
#' invisibly without evaluating the code.
#'
#' @param ... Either a single braced expression containing the code block to
#'   skip, or a character string followed by a single braced expression.
#'   When a message is supplied, it is printed instead of the default message.
#'
#' @return `NULL`, invisibly.
#'
#' @details
#' `skip_block()` uses non-standard evaluation to capture the code block via
#' `substitute()`. The code block is never evaluated.
#'
#' Supplying more than two arguments, or supplying no code block, is an error.
#'
#' @examples
#' skip_block({
#'   Sys.sleep(10)
#' })
#'
#' skip_block("Skipping slow preprocessing step", {
#'     Sys.sleep(10)
#'   }
#' )
#' @export
skip_block <- function(...) {
  dots <- substitute(list(...))[-1]
  dl <- length(dots)

  if (dl == 0) {
    stop("skip_block() requires a code block")
  }

  if (dl == 1) {
    msg  <- "Skipping code block"
    expr <- dots[[1]]
  } else if (dl == 2) {
    msg  <- ...elt(1)
    expr <- dots[[2]]
  } else {
    stop("skip_block() accepts at most a message and one code block")
  }

  lines <- deparse(expr, width.cutoff = 500L)
  cli::cli_inform("{msg} ({length(lines)} line{?s})")
  invisible(NULL)
}
