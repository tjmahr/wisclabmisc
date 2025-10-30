test_that("trapezoid_auc() computes an area under the curve", {
  # Easy by-hand example
  x <- 0:4
  y <- x ^ 2
  heights <- c(.5, 2.5, 6.5, 12.5)
  expect_equal(trapezoid_auc(x, y), sum(heights))

  # Bigger example with negative xs
  x <- -20:20
  y <- x ^ 2
  heights <- (y[-1] + y[-length(y)]) / 2
  expect_equal(trapezoid_auc(x, y), sum(heights))

  # Bigger example with negative ys and fractional xs
  x <- (20:-20) / 10
  y <- x ^ 3 + 4 * x
  heights <- (y[-1] + y[-length(y)]) / 2
  expect_equal(trapezoid_auc(x, y), sum(heights))
})

test_that("is_sorted() works on sorted vectors", {
  # strictly increasing
  expect_true(is_sorted(c(-3, -2, -1)))
  expect_true(is_sorted(c(1, 2, 3)))

  # strictly decreasing
  expect_true(is_sorted(c(3, 2, 1)))
  expect_true(is_sorted(c(-1, -2, -3)))

  # not strictly increasing
  expect_true(is_sorted(c(1, 1, 1)))
  expect_true(is_sorted(c(1, 1, 2)))
  expect_true(is_sorted(c(-1, 0, 0)))

  # not strictly decreasing
  expect_true(is_sorted(c(1, 1, 1)))
  expect_true(is_sorted(c(-1, -1, -2)))
  expect_true(is_sorted(c(1, 0, 0)))
})

test_that("is_sorted() works on unsorted vectors", {
  # spike up
  expect_false(is_sorted(c(1, 2, 1)))
  expect_false(is_sorted(c(-1, 2, -1)))

  # spike down
  expect_false(is_sorted(c(1, -2, 1)))
  expect_false(is_sorted(c(-1, -2, -1)))
})

test_that("other is_sorted() behavior is documented", {
  # length 1 is sorted
  expect_true(is_sorted(0))
  expect_true(is_sorted(NA))

  # NA when elements cannot be compared
  expect_true(is.na(is_sorted(c(NA, NA))))
})


test_that("is_increasing() and is_decreasing() work", {
  s <- c(1, 0, 1, 0)
  expect_false(is_increasing(s))
  expect_false(is_decreasing(s))

  s <- c(-2, -1, 0, 1, 10)
  expect_true( is_increasing(s))
  expect_false(is_decreasing(s))

  s <- c(2, 1, 0, -1, -10)
  expect_false(is_increasing(s))
  expect_true( is_decreasing(s))

  s <- c(1, 1, 1, 1, 1)
  expect_false(is_increasing(s))
  expect_false(is_decreasing(s))
})



