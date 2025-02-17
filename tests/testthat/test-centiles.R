test_that("multiplication works", {
  data <- data.frame(
    x = 1:16,
    c25 = 4,
    c50 = 8.5,
    c75 = 12
  )

  d1 <- data |>
    check_computed_centiles(x)

  d1$n_under_centile |> expect_equal(c(4, 8, 12))
  d1$n |> expect_equal(c(16, 16, 16))
  d1$percent_under_centile |> expect_equal(c(4, 8, 12) / 16 * 100)

  data_good <- data
  data_good$group <- "good"

  data_bad <- data
  data_bad$group <- "bad"
  data_bad$c25 <- 2
  data_bad$c50 <- 10
  data_bad$c75 <- 12

  data_grouped <- rbind(data_bad, data_good)

  d2 <- data_grouped |>
    check_computed_centiles(x)

  d2$n_under_centile |> expect_equal(c(6, 18, 24))
  d2$percent_under_centile |> expect_equal(c(6, 18, 24) / 32 * 100)

  d3 <- data_grouped |>
    dplyr::group_by(group) |>
    check_computed_centiles(x)

  d3$n_under_centile |> expect_equal(c(2, 10, 12, 4, 8, 12))
  d3$percent_under_centile |> expect_equal(c(2, 10, 12, 4, 8, 12) / 16 * 100)
})
