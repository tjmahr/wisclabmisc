## code to prepare `example_rates` dataset goes here

data_fake_rates <- readr::read_csv(
  "data-raw/example-rates.csv",
  col_types = "in"
) |>
  tibble::as_tibble()

usethis::use_data(data_fake_rates, overwrite = TRUE)
