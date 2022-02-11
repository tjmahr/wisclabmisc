test_roc_results <- function(base_roc, our_roc) {
  expect_equal(base_roc$sensitivities, our_roc$.sensitivities)
  expect_equal(base_roc$specificities, our_roc$.specificities)
  expect_equal(base_roc$thresholds, our_roc[[1]])

  expect_equal(base_roc$direction, our_roc$.direction[1])
  expect_equal(as.numeric(pROC::auc(base_roc)), our_roc$.auc[1])
}

test_roc_levels <- function(roc, levels) {
  expect_equal(roc$.controls[1], levels[1])
  expect_equal(roc$.cases[1], levels[2])
}

# I am not using a helper function to compute pairs of ROC curves because of the
# nonstandard evaluation used to compute the curves. We just copy and paste a
# bit.



test_that("compute_empirical_roc() with auto", {
  data <- as_tibble(pROC::aSAH)
  dir <- "auto"
  suppressMessages({
    base_roc <- pROC::roc(data, outcome, s100b, direction = dir)
    our_roc <- compute_empirical_roc(data, outcome, s100b, direction = dir)
  })
  test_roc_results(base_roc, our_roc)


  levels <- c("Good", "Poor")
  suppressMessages({
    base_roc <- pROC::roc(
      data, outcome, s100b, direction = dir, levels = levels
    )
    our_roc <- compute_empirical_roc(
      data, outcome, s100b, direction = dir, levels = levels
    )
  })
  test_roc_results(base_roc, our_roc)
  test_roc_levels(our_roc, levels)


  levels <- rev(levels)
  suppressMessages({
    base_roc <- pROC::roc(
      data, outcome, s100b, direction = dir, levels = levels
    )
    our_roc <- compute_empirical_roc(
      data, outcome, s100b, direction = dir, levels = levels
    )
  })
  test_roc_results(base_roc, our_roc)
  test_roc_levels(our_roc, levels)
})



test_that("compute_empirical_roc() with <", {
  data <- as_tibble(pROC::aSAH)
  dir <- "<"
  suppressMessages({
    base_roc <- pROC::roc(data, outcome, s100b, direction = dir)
    our_roc <- compute_empirical_roc(data, outcome, s100b, direction = dir)
  })
  test_roc_results(base_roc, our_roc)


  levels <- c("Good", "Poor")
  suppressMessages({
    base_roc <- pROC::roc(
      data, outcome, s100b, direction = dir, levels = levels
    )
    our_roc <- compute_empirical_roc(
      data, outcome, s100b, direction = dir, levels = levels
    )
  })
  test_roc_results(base_roc, our_roc)
  test_roc_levels(our_roc, levels)


  levels <- rev(levels)
  suppressMessages({
    base_roc <- pROC::roc(
      data, outcome, s100b, direction = dir, levels = levels
    )
    our_roc <- compute_empirical_roc(
      data, outcome, s100b, direction = dir, levels = levels
    )
  })
  test_roc_results(base_roc, our_roc)
  test_roc_levels(our_roc, levels)
})



test_that("compute_empirical_roc() with >", {
  data <- as_tibble(pROC::aSAH)
  dir <- ">"
  suppressMessages({
    base_roc <- pROC::roc(data, outcome, s100b, direction = dir)
    our_roc <- compute_empirical_roc(data, outcome, s100b, direction = dir)
  })
  test_roc_results(base_roc, our_roc)


  levels <- c("Good", "Poor")
  suppressMessages({
    base_roc <- pROC::roc(
      data, outcome, s100b, direction = dir, levels = levels
    )
    our_roc <- compute_empirical_roc(
      data, outcome, s100b, direction = dir, levels = levels
    )
  })
  test_roc_results(base_roc, our_roc)
  test_roc_levels(our_roc, levels)

  levels <- rev(levels)
  suppressMessages({
    base_roc <- pROC::roc(
      data, outcome, s100b, direction = dir, levels = levels
    )
    our_roc <- compute_empirical_roc(
      data, outcome, s100b, direction = dir, levels = levels
    )
  })
  test_roc_results(base_roc, our_roc)
  test_roc_levels(our_roc, levels)
})



