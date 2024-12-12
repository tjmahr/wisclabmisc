

test_that("info_surprisal() computes correct values", {
  expect_equal(info_surprisal(1), 0)
  expect_equal(info_surprisal(1 / c(2, 4)), log(c(2, 4)))
})

test_that("info_entropy() computes correct entropy for distributions", {
  expect_equal(
    info_entropy(c(1 / 4, 3 / 4)),
    -(1 / 4 * log(1 / 4) + 3 / 4 * log(3 / 4))
  )
  expect_equal(info_entropy(rep(1 / 4, 4)), log(4))
  expect_error(info_entropy(c(.4, .4, .4)))
})

test_that("info_cross_entropy() computes correct cross entropy for distributions", {
  p <- c(1 / 2, 1 / 2)
  q <- c(1 / 4, 3 / 4)
  expect_equal(info_cross_entropy(p, q), sum(p * -log(q)))
})

test_that("info_kl_divergence() computes correct KL divergence for distributions", {
  p <- c(1 / 2, 1 / 2)
  q <- c(1 / 4, 3 / 4)
  expect_equal(info_kl_divergence(p, q), sum(p * log(p / q)))
  expect_equal(info_kl_divergence(p, p), 0)
})
