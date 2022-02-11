## code to prepare `intelligibility_growth` dataset goes here

intelligibility_growth <- readr::read_csv(
  "data-raw/intelligibility-by-age.csv",
  col_types = "iddc"
)
usethis::use_data(intelligibility_growth, overwrite = TRUE)
