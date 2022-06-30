## code to prepare `example_rates` dataset goes here

data_fake_intelligibility <- readr::read_csv(
  "data-raw/example-intelligibility.csv",
  col_types = "in"
)
usethis::use_data(data_fake_intelligibility, overwrite = TRUE)
