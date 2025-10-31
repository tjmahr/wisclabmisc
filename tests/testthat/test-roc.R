
test_that("compute_sens_spec_from_ecdf() works on weighted/unweighted data", {
  # # denominators
  # tapply(df$w, df$grp, sum)
  # tapply(df$w, df$grp, length)

  df <- data.frame(
    grp = c("ctrl", "ctrl", "ctrl", "case", "case", "case", "case"),
    x   = c(0, 1, 3, 2, 4, 5, 5),
    w   = c(2, 1, 2, 1, 1, 1, 1),
    # Work out the ECDF by hand
    f1  = c(0/4, 0/4, 1/4, 1/4, 2/4, 4/4, 4/4),
    f2  = 1 - c(2/5, 3/5, 5/5, 3/5, 5/5, 5/5, 5/5),
    # Without weights
    g1  = c(0/4, 0/4, 1/4, 1/4, 2/4, 4/4, 4/4),
    g2  = 1 - c(1/3, 2/3, 3/3, 2/3, 3/3, 3/3, 3/3)
  )

  exp_vals <- df |> sort_by(~x) |> unique()

  out <- compute_sens_spec_from_ecdf(
    data = df,
    response = grp,
    predictor = x,
    weights = w,
    direction = ">",
    levels = c("ctrl", "case")
  )

  expect_equal(out[["x"]], c(-Inf, 0:5, Inf))
  expect_true(is_increasing(out$.sensitivities))
  expect_true(is_decreasing(out$.specificities))
  expect_equal(out$.sensitivities, c(0, exp_vals$f1, 1))
  expect_equal(out$.specificities, c(1, exp_vals$f2, 0))

  # Flip the direction
  out_flip <- compute_sens_spec_from_ecdf(
    data = df,
    response = grp,
    predictor = x,
    weights = w,
    direction = "<",
    levels = c("ctrl", "case")
  )
  expect_equal(out_flip$.sensitivities, 1 - c(0, exp_vals$f1, 1))
  expect_equal(out_flip$.specificities, 1 - c(1, exp_vals$f2, 0))

  # Unweighted is like weighting by 1
  out_no_wt <- compute_sens_spec_from_ecdf(
    data = df,
    response = grp,
    predictor = x,
    weights = NULL,
    direction = ">",
    levels = c("ctrl", "case")
  )
  expect_equal(out_no_wt$.sensitivities, c(0, exp_vals$g1, 1))
  expect_equal(out_no_wt$.specificities, c(1, exp_vals$g2, 0))

  exp_vals <- df |> sort_by(~x) |> unique()

  # Simple integer weights are like repeating observations
  df2 <- data.frame(
    grp = rep(df$grp, df$w),
    x = rep(df$x, df$w),
    w = 1
  )

  out_wt_by_rep <- compute_sens_spec_from_ecdf(
    data = df2,
    response = grp,
    predictor = x,
    weights = w,
    direction = ">",
    levels = c("ctrl", "case")
  )
  expect_equal(out_wt_by_rep$.sensitivities, c(0, exp_vals$f1, 1))
  expect_equal(out_wt_by_rep$.specificities, c(1, exp_vals$f2, 0))
})



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

  # length 0 is sorted
  expect_true(is_sorted(integer(0)))

  # NA when elements cannot be compared
  expect_true(is.na(is_sorted(c(NA, NA))))
})


test_that("is_increasing() and is_decreasing() work", {
  negative_cases <- list(
    length_0 = numeric(0),
    length_1 = 1,
    length_1_with_NA = NA_integer_,
    length_2_with_NA = c(1, NA),
    length_3_with_NA = c(1, 2, NA),
    constant = c(1, 1, 1, 1),
    not_monotonic = c(3, 2, 1, 2)
  )

  for (v in negative_cases) {
    expect_false(is_increasing(v))
    expect_false(is_increasing(v))
    expect_false(is_decreasing(v))
  }

  positive_cases <- list(
    strictly = c(1, 2, 3, 4),
    plateau_1 = c(1, 2, 2, 2),
    plateau_2 = c(1, 1, 1, 2)
  )

  for (v in positive_cases) {
    expect_true(is_increasing(v))
    expect_false(is_decreasing(v))
    v <- rev(v)
    expect_true(is_decreasing(v))
    expect_false(is_increasing(v))
  }
})



