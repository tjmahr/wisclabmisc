test_that("inspect_number() handles NULL according to allow_null", {
  expect_error(inspect_number(NULL, allow_null = FALSE), "must not be NULL")
  expect_no_error(inspect_number(NULL, allow_null = TRUE))
})

test_that("inspect_number() rejects non-numeric inputs", {
  expect_error(inspect_number("1"), "must be a numeric vector")
  expect_error(inspect_number(list(1)), "must be a numeric vector")
})

test_that("inspect_number() rejects empty vectors", {
  expect_error(inspect_number(numeric()), "must not be empty")
  expect_error(inspect_number(integer()), "must not be empty")
})

test_that("inspect_number() enforces scalar vs vector when allow_vector = FALSE", {
  expect_error(inspect_number(1:2, allow_vector = FALSE), "must be a scalar")
  expect_no_error(inspect_number(1, allow_vector = FALSE))
})

test_that("inspect_number() enforces missingness via allow_missing", {
  x <- c(1, NA_real_, 3)
  expect_error(inspect_number(x, allow_missing = FALSE), "must not contain missing")
  expect_no_error(inspect_number(x, allow_missing = TRUE))

  y <- c(1, NaN, 3)
  expect_error(inspect_number(y, allow_missing = FALSE), "must not contain missing")
  expect_no_error(inspect_number(y, allow_missing = TRUE))
})

test_that("inspect_number() enforces infinite policy via allow_infinite", {
  x <- c(1, Inf, 3)
  expect_error(inspect_number(x, allow_infinite = FALSE), "must not contain infinite")
  expect_no_error(inspect_number(x, allow_infinite = TRUE))

  y <- c(1, -Inf, 3)
  expect_error(inspect_number(y, allow_infinite = FALSE), "must not contain infinite")
  expect_no_error(inspect_number(y, allow_infinite = TRUE))
})

test_that("inspect_number() whole-number check ignores non-finite values", {
  x <- c(NA_real_, 1, 2, Inf)

  expect_no_error(
    inspect_number(x, allow_infinite = TRUE, allow_missing = TRUE, allow_decimals = FALSE),
  )
})

test_that("inspect_number() enforces allow_decimals = FALSE for non-integers", {
  expect_error(inspect_number(c(1, 2.5), allow_decimals = FALSE), "whole numbers")
  expect_no_error(inspect_number(c(1, 2), allow_decimals = FALSE))

  # Integer storage should always pass
  expect_no_error(inspect_number(as.integer(c(1, 2)), allow_decimals = FALSE))
})


test_that("inspect_number() lower bound inclusive/exclusive/tolerant behave", {
  c(0, 1, 2) |>
    inspect_number(min = 0, min_boundary = "tolerant") |>
    expect_no_error() |>
    inspect_number(min = 0, min_boundary = "inclusive") |>
    expect_no_error() |>
    inspect_number(min = 0, min_boundary = "exclusive") |>
    expect_error("less than")

  tol <- 1e-6
  c(-5e-7, 0, 1) |>
    inspect_number(min = 0, min_boundary = "tolerant", tolerance = tol) |>
    expect_no_error() |>
    inspect_number(min = 0, min_boundary = "inclusive", tolerance = tol) |>
    expect_error("less than")
})

test_that("inspect_number() upper bound inclusive/exclusive/tolerant behave", {
  c(0, 1, 2) |>
    inspect_number(max = 2, max_boundary = "tolerant") |>
    expect_no_error() |>
    inspect_number(max = 2, max_boundary = "inclusive") |>
    expect_no_error() |>
    inspect_number(max = 2, max_boundary = "exclusive") |>
    expect_error("greater than")

  tol <- 1e-6
  c(0, 1, 2, 2 + 5e-7) |>
    inspect_number(max = 2, max_boundary = "tolerant", tolerance = tol) |>
    expect_no_error() |>
    inspect_number(max = 2, max_boundary = "inclusive", tolerance = tol) |>
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

test_that("is_oob_lower/upper return correct logical vectors", {
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
