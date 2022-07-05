test_that("try_uniroot() returns NAs", {
  f <- function(x) log(x) + 1

  # Works on safe cases
  okay <- stats::uniroot(f, c(.1, 10))
  try_okay <- try_uniroot(f, c(.1, 10))
  expect_equal(okay, try_okay)

  # Gives NAs on bad cases
  try_fail <- expect_warning(try_uniroot(f, c(-.1, 10)))
  try_fail <- suppressWarnings(try_uniroot(f, c(-.1, 10)))
  expect_true(is.na(try_fail$root))
})
