test_that("compute_smooth_density_roc() correctly orders the sensitivity", {
  # Our tidying function always returns the data increasing order along x or in
  # the direction of the original data. So we need to test that the ordering of
  # the sensitivities makes sense.
  is_in_expected_direction <- function(data_smooth, expected_direction) {
    first_col <- names(data_smooth[1])

    data_smooth <- data_smooth  %>%
      filter(!is.na(.data[[first_col]]))

    dir <- data_smooth[[".direction"]][1]
    expect_equal(dir, expected_direction)

    # for these tests, the threshold variable is in increasing order
    expect_true(is_increasing(data_smooth[[first_col]]))
    expect_equal(
      is_increasing(data_smooth[[".sensitivities"]]),
      is_decreasing(data_smooth[[".specificities"]])
    )

    if (dir == "<") {
      # controls *<* t <= cases, so sens = 1 at x[1]
      expect_true(is_decreasing(data_smooth[[".sensitivities"]]))
    } else {
      # controls *>* t >= cases, so sens = 0 at x[1]
      expect_true(is_increasing(data_smooth[[".sensitivities"]]))
    }

    data_smooth
  }

  set.seed(100)
  a <- -abs(rnorm(100, -5, 2))
  b <- abs(rnorm(20, 6, 2))
  x <- seq(min(a) - .1, max(b) + .1, length.out = 200)

  # data <- tibble::tibble(
  #   response = c(rep("a", 100), rep("b", 20)),
  #   predictor = c(a, b)
  # )

  dens <- tibble::tibble(
    x = x,
    x_neg = -1 * x,
    d_a = dnorm(x, mean(a), sd(a)),
    d_b = dnorm(x, 6, 2)
  )

  # "auto" will choose the "<" direction. it's not clear how it make this
  # decision.
  smooth_roc_auto <- dens %>%
    compute_smooth_density_roc(d_a, d_b, x, direction = "auto") %>%
    is_in_expected_direction("<")

  # this is the same as above
  smooth_roc_lt <- dens %>%
    compute_smooth_density_roc(d_a, d_b, x, direction = "<") %>%
    is_in_expected_direction("<")

  # now the direction is >, so the sensitivities increase with x
  smooth_roc_gt <- dens %>%
    compute_smooth_density_roc(d_a, d_b, x, direction = ">") %>%
    is_in_expected_direction(">")

  # `along` variable determines the sorting, so negative x flips direction for
  # auto.
  smooth_roc_neg_auto <- dens %>%
    compute_smooth_density_roc(d_a, d_b, x_neg, direction = "auto") %>%
    select(x_neg, everything()) %>%
    arrange(x_neg) %>%
    is_in_expected_direction(">")

  # but the ordering respects the ordering of the `along` variable
  smooth_roc_neg_lt <- dens %>%
    compute_smooth_density_roc(d_a, d_b, x_neg, direction = "<") %>%
    select(x_neg, everything()) %>%
    arrange(x_neg) %>%
    is_in_expected_direction("<")

  smooth_roc_gt <- dens %>%
    compute_smooth_density_roc(d_a, d_b, x_neg, direction = ">") %>%
    select(x_neg, everything()) %>%
    arrange(x_neg) %>%
    is_in_expected_direction(">")

  # if we flip case/control columns, "auto" direction should flip to ">"
  smooth_roc_auto_flip <- dens %>%
    compute_smooth_density_roc(d_b, d_a, x, direction = "auto") %>%
    is_in_expected_direction(">")

  expect_equal(
    is_increasing(smooth_roc_auto_flip$.sensitivities),
    is_decreasing(smooth_roc_auto$.sensitivities)
  )

  # same as above
  smooth_roc_gt_flip <- dens %>%
    compute_smooth_density_roc(d_b, d_a, x, direction = ">") %>%
    is_in_expected_direction(">")

  smooth_roc_lt_flip <- dens %>%
    compute_smooth_density_roc(d_b, d_a, x, direction = "<") %>%
    is_in_expected_direction("<")
})




test_that("we understand how roc direction works", {
  drop_first_last_rows <- function(data) {
    data[-unique(c(1, nrow(data))), ]
  }

  set.seed(100)
  a <- -abs(rnorm(100, -5, 2))
  b <- abs(rnorm(20, 6, 2))
  x <- seq(min(a) - .1, max(b) + .1, length.out = 200)

  data <- tibble::tibble(
    response = c(rep("a", 100), rep("b", 20)),
    predictor = c(a, b)
  )

  # If the controls are a, then direction should be controls *<* t <= cases.
  # sensitivity should be 1 at x[1] because all cases are above this point.
  emp_lt <- pROC::roc(
    data,
    response,
    predictor,
    levels = c("a", "b"),
    direction = "<"
  )

  coord_lt <- emp_lt %>%
    pROC::coords() %>%
    drop_first_last_rows()

  # test our understanding
  expect_true(is_increasing(coord_lt$threshold))
  expect_true(is_decreasing(coord_lt$sensitivity))
  expect_equal(emp_lt$direction, "<")

  # If the controls are d_b, then direction should be controls *>* t >= cases.
  # The sensitivity at the last x should be 1 because all controls are above
  # this point.
  emp_gt <- pROC::roc(
    data,
    response,
    predictor,
    levels = c("b", "a"),
    direction = ">"
  )

  coord_gt <- emp_gt %>%
    pROC::coords() %>%
    drop_first_last_rows()

  # test our understanding
  expect_true(is_decreasing(coord_gt$threshold))
  expect_true(is_decreasing(coord_gt$sensitivity))
  expect_equal(emp_gt$direction, ">")


})
