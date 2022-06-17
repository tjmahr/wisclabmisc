## code to prepare `example_rates` dataset goes here

data_fake_rates <- readr::read_csv(
  "data-raw/example_rates.csv",
  col_types = "in"
)
usethis::use_data(data_fake_rates, overwrite = TRUE)
