

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




#' Compute chronological age in months
#'
#' Ages are rounded down to the nearest month. A difference of 20 months, 29
#' days is interpreted as 20 months.
#'
#' @param t1,t2 dates in "yyyy-mm-dd" format
#' @return the chronological ages in months. NA is returned if the age cannot be
#'   computed.
#' @export
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
#' are removed.
#' @rdname file_rename_with
#' @details Only the basename of the file (returned by [basename()]
#' undergoes string replacement).
#'
#' @examples
#' # With .dry_run = TRUE, we can make up some file paths.
#' dir <- "//some-fake-location/"
#' path <- fs::path(
#'   dir,
#'   c("report_1.csv", "report_2.csv", "report-1.csv", "skipped.csv")
#' )
#'
#' updated <- file_replace_name(path, "report_", "report-", .dry_run = TRUE)
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
  # basename_new <- stringr::str_replace(basename_old, pattern, replacement)
  basename_new <- .fn(basename_old, ...)
  path_new <- fs::path(fs::path_dir(path_old), basename_new)

  changed <- path_old != path_new
  name_already_found <- path_new %in% path_old
  is_an_overwrite <- changed & name_already_found

  if (!.overwrite & !.dry_run) {
    cli::cli_abort(
      message = c(
        "{sum(is_an_overwrite)} file{?s} would be overwritten",
        "*" = "Set {.code .overwrite = TRUE} to deliberately overwrite files",
        "*" = "Set {.code .dry_run = TRUE} to see affected files"
      ),
      call = NULL
    )
  }

  if (.dry_run) {
    over_msgs <- rep(" {.emph (overwrites an existing file)}", length(path_new))
    over_msgs[!is_an_overwrite] <- ""
    changes <- sprintf(
      "%s -> %s%s",
      basename(path_old)[changed],
      basename(path_new)[changed],
      over_msgs[changed]
    )
    names(changes) <- rep(" ", length(changes))
    cli::cli_inform(c("Planned changes:", changes))
    invisible(path_old)
  } else {
    fs::file_move(path_old[changed], path_new[changed])
    invisible(unique(path_new))
  }
}
