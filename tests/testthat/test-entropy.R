

test_that("info_surprisal() computes correct values", {
  expect_equal(info_surprisal(1), 0)
  expect_equal(info_surprisal(1, 2), 0)
  expect_equal(info_surprisal(1 / c(2, 4)), log(c(2, 4)))

  expect_equal(info_surprisal(c(1, NA)), c(0, NA_real_))
  expect_error(info_surprisal(-.01))
  expect_error(info_surprisal(1.1))
})

test_that("info_entropy() computes correct entropy for distributions", {
  expect_equal(
    info_entropy(c(1 / 4, 3 / 4)),
    -(1 / 4 * log(1 / 4) + 3 / 4 * log(3 / 4))
  )
  expect_equal(info_entropy(rep(1 / 4, 4)), log(4))

  # SciPy example
  c(.9, .1) |>
    info_entropy(base = 2) |>
    round(3) |>
    expect_equal(.469)

  # wikipedia examples
  c(.7, .3) |>
    info_entropy(base = 2) |>
    round(3) |>
    expect_equal(.881)

  rep(1/3, 3) |>
    info_entropy(base = 2) |>
    round(5) |>
    expect_equal(1.58496)

  # Sum > 1, negative probability, probability > 1
  expect_error(info_entropy(c( .4, .4,  .4)))
  expect_error(info_entropy(c( .5, .5, -.5)))
  expect_error(info_entropy(c(1.1, .4,  .4)))

  # do we want an error or NA
  expect_error(info_entropy(c(.1, .9, NA)))
})

test_that("info_cross_entropy() computes correct cross entropy for distributions", {
  p <- c(1 / 2, 1 / 2)
  q <- c(1 / 4, 3 / 4)
  expect_equal(info_cross_entropy(p, q), sum(p * -log(q)))

  # SciPy example
  p <- c(.5, .5)
  q <- c(.9, .1)
  info_cross_entropy(p, q, 2) |>
    round(3) |>
    expect_equal(1.737)

  # Sum > 1, negative probability, probability > 1
  expect_error(info_cross_entropy(c( .4, .4,  .4), c(.3, .5, .2)))
  expect_error(info_cross_entropy(c( .5, .5, -.5), c(.3, .5, .2)))
  expect_error(info_cross_entropy(c(1.1, .4,  .4), c(.3, .5, .2)))

  expect_error(info_cross_entropy(c(.3, .5, .2), c( .4, .4,  .4)))
  expect_error(info_cross_entropy(c(.3, .5, .2), c( .5, .5, -.5)))
  expect_error(info_cross_entropy(c(.3, .5, .2), c(1.1, .4,  .4)))
})

test_that("info_kl_divergence() computes correct KL divergence for distributions", {
  p <- c(1 / 2, 1 / 2)
  q <- c(1 / 4, 3 / 4)
  expect_equal(info_kl_divergence(p, q), sum(p * log(p / q)))
  expect_equal(info_kl_divergence(p, p), 0)

  wikipedia_example <- rbind(
    p = c(9, 12, 4) / 25,
    q = c(1,  1, 1) / 3
  )
  kl <- c(.0852996, .097455)

  info_kl_divergence(wikipedia_example[1, ], wikipedia_example[2, ]) |>
    round(7) |>
    expect_equal(kl[1])

  info_kl_divergence(wikipedia_example[2, ], wikipedia_example[1, ]) |>
    round(7) |>
    expect_equal(kl[2])

  info_kl_divergence_matrix(wikipedia_example) |>
    round(7) |>
    unname() |>
    expect_equal(
      matrix(c(0, kl[1], kl[2], 0), nrow = 2, byrow = TRUE)
    )

  # Sum > 1, negative probability, probability > 1
  expect_error(info_kl_divergence(c( .4, .4,  .4), c(.3, .5, .2)))
  expect_error(info_kl_divergence(c( .5, .5, -.5), c(.3, .5, .2)))
  expect_error(info_kl_divergence(c(1.1, .4,  .4), c(.3, .5, .2)))

  expect_error(info_kl_divergence(c(.3, .5, .2), c( .4, .4,  .4)))
  expect_error(info_kl_divergence(c(.3, .5, .2), c( .5, .5, -.5)))
  expect_error(info_kl_divergence(c(.3, .5, .2), c(1.1, .4,  .4)))

})
