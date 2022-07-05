

testthat::test_that("Predictive values example from Wikipedia work", {
  # https://en.wikipedia.org/wiki/Bayes%27_theorem#Drug_testing
  ppv <- compute_predictive_value_from_rates(
    sensitivity = .9,
    specificity = .8,
    prevalence = .05
  )
  expect_equal(round(ppv$ppv, 2), .19)

  # https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values#Worked_example
  ppv2 <- compute_predictive_value_from_rates(
    sensitivity = .667,
    specificity = .91,
    prevalence = .0148
  )
  expect_equal(round(ppv2$ppv, 3), .1)
  expect_equal(round(ppv2$npv, 4), .9945)

  # test recycling
  ppv3 <- compute_predictive_value_from_rates(
    sensitivity = c(.9, .667),
    specificity = c(.8, .91),
    prevalence =  c(.05, .0148)
  )

  expect_equal(round(ppv3$ppv[1], 2), .19)
  expect_equal(round(ppv3$ppv[2], 3), .1)
  expect_equal(round(ppv3$npv[2], 4), .9945)

  # test recycling
  expect_error(
    compute_predictive_value_from_rates(
      sensitivity = c(.9, .667, .001),
      specificity = c(.8, .91),
      prevalence =  c(.05)
    )
  )
})
