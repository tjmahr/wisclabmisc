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


test_that("tocs_item()", {
  x <- c(
    "XXv17_SLOW_s4T06.TextGrid",
    "XXv14_SLOW_s5T04.TextGrid",
    "XXv16s6T06.TextGrid",
    "XX(b)v16s7T03.TextGrid",
    "XXv15_SLOW_s2T01.TextGrid",
    "XXv18wT11.wav",
    "XXv16s7T06.lab",
    "XXv15s5T06.lab",
    "XXv13s3T10.WAV",
    "XXv16S2T09.wav",
    "s2T01",
    "invalid-file.txt",
    "XXv01s4B01.wav",
    "XXv01wB01.wav"
  )

  expected <- c(
    "S4T06",
    "S5T04",
    "S6T06",
    "S7T03",
    "S2T01",
    "WT11",
    "S7T06",
    "S5T06",
    "S3T10",
    "S2T09",
    "S2T01",
    NA_character_,
    "S4B01",
    "WB01"
  )

  expected_length <- c(
    4,
    5,
    6,
    7,
    2,
    1,
    7,
    5,
    3,
    2,
    2,
    NA_integer_,
    4,
    1
  )

  x |>
    tocs_item() |>
    expect_equal(expected) |>
    # i.e., item parsing works on standardized item names
    tocs_item() |>
    expect_equal(expected)

  x |>
    tocs_length() |>
    expect_equal(expected_length)
})
