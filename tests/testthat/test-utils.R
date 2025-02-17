test_that("Age formatting works", {
  c(1, 13, 23, 24, NA) |>
    format_year_month_age() |>
    expect_equal(c("0;1", "1;1", "1;11", "2;0", "NA;NA"))
})


test_that("Chronological age in months", {
  # Two years exactly
  expect_equal(chrono_age("2014-01-20", "2012-01-20"), 24)

  # Shift a year
  expect_equal(chrono_age("2014-01-20", "2013-01-20"), 12)
  expect_equal(chrono_age("2014-01-20", "2011-01-20"), 36)

  # Shift a month
  expect_equal(chrono_age("2014-01-20", "2012-02-20"), 23)
  expect_equal(chrono_age("2014-01-20", "2011-12-20"), 25)

  # Multiple dates
  t1 <- c("2014-01-20", "2014-01-20")
  t2 <- c("2012-02-20", "2011-12-20")
  expect_equal(chrono_age(t1, t2), c(23, 25))

  # Missing dates raise warnings and default to NA
  expect_warning(chrono_age(NA, "2011-12-20"))
  expect_true(is.na(suppressWarnings(chrono_age(NA, "2011-12-20"))))

  # 3 months exactly
  expect_equal(chrono_age("2014-05-10", "2014-02-10"), 3)

  # Borrow a month when the earlier date has a later day
  expect_equal(chrono_age("2014-05-10", "2014-02-11"), 2)

  # Reversed arguments
  expect_equal(chrono_age("2012-01-20", "2014-01-20"), 24)

  # Checks against EVT/PPVT examples
  expect_equal(chrono_age("2007-06-29", "2001-03-03"),  (6 * 12) + 3)
  expect_equal(chrono_age("2007-05-08", "1999-08-02"),  (7 * 12) + 9)
  expect_equal(chrono_age("2007-08-13", "1985-11-24"), (21 * 12) + 8)
})

test_that("File renaming functions", {

  dir <- tempfile()
  dir.create(dir)
  dir |>
    file.path(
      c("report_1.csv", "report_2.csv", "report-1.csv", "skipped.csv")
    ) |>
    file.create()

  path <- list.files(dir, full.names = TRUE)

  path |>
    file_replace_name("report_", "report-") |>
    expect_error(regexp = "would be overwritten")

  path |>
    file_replace_name("report_", "report-", .dry_run = TRUE) |>
    expect_message(regexp = "overwrites an existing file")

  updated <- file_replace_name(path, "report_", "report-", .overwrite = TRUE)
  new_path <- list.files(dir, full.names = TRUE)

  new_path |>
    basename() |>
    expect_equal(c("report-1.csv", "report-2.csv", "skipped.csv"))
})

