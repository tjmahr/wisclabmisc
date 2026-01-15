test_that("inspect_number() handles `NULL` with `allow_null`", {
  NULL |>
    inspect_number(allow_null = TRUE) |>
    expect_no_error() |>
    inspect_number(allow_null = FALSE) |>
    expect_error("must not be NULL")
})

test_that("inspect_number() rejects non-numeric inputs", {
  "1" |>
    inspect_number() |>
    expect_error("must be a numeric vector")

  list(1) |>
    inspect_number() |>
    expect_error("must be a numeric vector")
})

test_that("inspect_number() rejects empty vectors", {
  numeric(0) |>
    inspect_number() |>
    expect_error("must not be empty")

  integer(0) |>
    inspect_number() |>
    expect_error("must not be empty")
})

test_that("inspect_number() enforces scalar vs vector with `allow_vector`", {
  c(1:2) |>
    inspect_number(allow_vector = TRUE) |>
    expect_no_error() |>
    inspect_number(allow_vector = FALSE) |>
    expect_error("must be a scalar")

  1L |>
    inspect_number(allow_vector = TRUE) |>
    expect_no_error() |>
    inspect_number(allow_vector = FALSE) |>
    expect_no_error()
})

test_that("inspect_number() enforces non-missingness with `allow_missing`", {
  c(1, NA_real_, 3) |>
    inspect_number(allow_missing = TRUE) |>
    expect_no_error() |>
    inspect_number(allow_missing = FALSE) |>
    expect_error("must not contain missing")

  c(1, NaN, 3) |>
    inspect_number(allow_missing = TRUE) |>
    expect_no_error() |>
    inspect_number(allow_missing = FALSE) |>
    expect_error("must not contain missing")
})

test_that("inspect_number() enforces infinite policy with `allow_infinite`", {
  c(1, Inf, 3) |>
    inspect_number(allow_infinite = TRUE) |>
    expect_no_error() |>
    inspect_number(allow_infinite = FALSE) |>
    expect_error("must not contain infinite")

  c(1, -Inf, 3) |>
    inspect_number(allow_infinite = TRUE) |>
    expect_no_error() |>
    inspect_number(allow_infinite = FALSE) |>
    expect_error("must not contain infinite")
})

test_that("inspect_number() enforces integer-vs-double with `allow_decimals`", {
  c(1, 2.5) |>
    inspect_number(allow_decimals = TRUE) |>
    expect_no_error() |>
    inspect_number(allow_decimals = FALSE) |>
    expect_error("whole numbers")

  c(1, 0.0) |>
    inspect_number(allow_decimals = TRUE) |>
    expect_no_error() |>
    inspect_number(allow_decimals = FALSE) |>
    expect_no_error()
})

test_that("inspect_number() whole-number check ignores non-finite values", {
  c(NA_real_, 1, 2, Inf) |>
    inspect_number(
      allow_infinite = TRUE,
      allow_missing = TRUE,
      allow_decimals = FALSE
    ) |>
    expect_no_error()
})

test_that("inspect_number() enforces boundary constraints", {
  tol <- 1e-6

  # x <? Lower
  c(0, 1, 2) |>
    inspect_number(min = 0, min_boundary = "tolerant", tolerance = tol) |>
    expect_no_error() |>
    inspect_number(min = 0, min_boundary = "inclusive") |>
    expect_no_error() |>
    inspect_number(min = 0, min_boundary = "exclusive") |>
    expect_error("less than")

  # OOB but within tolerance
  c(-5e-7, 0, 1) |>
    inspect_number(min = 0, min_boundary = "tolerant", tolerance = tol) |>
    expect_no_error() |>
    inspect_number(min = 0, min_boundary = "inclusive") |>
    expect_error("less than")

  # Upper <? x
  c(0, 1, 2) |>
    inspect_number(max = 2, max_boundary = "tolerant", tolerance = tol) |>
    expect_no_error() |>
    inspect_number(max = 2, max_boundary = "inclusive") |>
    expect_no_error() |>
    inspect_number(max = 2, max_boundary = "exclusive") |>
    expect_error("greater than")

  # OOB but within tolerance
  c(0, 1, 2, 2 + 5e-7) |>
    inspect_number(max = 2, max_boundary = "tolerant", tolerance = tol) |>
    expect_no_error() |>
    inspect_number(max = 2, max_boundary = "inclusive") |>
    expect_error("greater than")
})

test_that("inspect_number() passes bound checks when min/max are NULL", {
  c(-100, 0, 100) |>
    inspect_number(min = NULL, max = NULL) |>
    expect_no_error() |>
    inspect_number(min = NULL, max = 100) |>
    expect_no_error() |>
    inspect_number(min = -100, max = NULL) |>
    expect_no_error()
})

test_that("is_oob_lower()/is_oob_upper() return logical vectors", {
  c(-1, 0, 1) |>
    is_oob_lower(min = 0, boundary = "inclusive") |>
    expect_equal(c(TRUE, FALSE, FALSE))

  c(-1, 0, 1) |>
    is_oob_lower(min = 0, boundary = "exclusive") |>
    expect_equal(c(TRUE, TRUE, FALSE))

  c(-1, 0, 1) |>
    is_oob_upper(max = 0, boundary = "inclusive") |>
    expect_equal(c(FALSE, FALSE, TRUE))

  c(-1, 0, 1) |>
    is_oob_upper(max = 0, boundary = "exclusive") |>
    expect_equal(c(FALSE, TRUE, TRUE))

  tol <- 1e-6
  c(-5e-7, 0, 5e-7) |>
    is_oob_lower(min = 0, boundary = "tolerant", tolerance = tol) |>
    expect_equal(c(FALSE, FALSE, FALSE))

  # exceeds tolerance
  c(0 - 2e-6, 0, 1) |>
    is_oob_lower(min = 0, boundary = "tolerant", tolerance = tol) |>
    expect_equal(c(TRUE, FALSE, FALSE))

  c(0, 2 + 5e-7, 2 + 2e-6) |>
    is_oob_upper(max = 2, boundary = "tolerant", tolerance = tol) |>
    expect_equal(c(FALSE, FALSE, TRUE))
})

test_that("inspect_number() returns x invisibly", {
  x <- c(1, 2, 3)
  out <- inspect_number(x)
  expect_identical(out, x)
  expect_invisible(inspect_number(x))
})
