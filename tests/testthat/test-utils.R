test_that("Age formatting works", {
  c(1, 13, 23, 24, NA) |>
    format_year_month_age() |>
    expect_equal(c("0;1", "1;1", "1;11", "2;0", "NA;NA"))
})
