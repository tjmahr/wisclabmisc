data_example_intelligibility_by_length <- readr::read_csv(
  "data-raw/intelligibility-epreds.csv",
  col_types = "ciicin"
) |>
  tibble::as_tibble() |>
  dplyr::select(-facet_lab)


usethis::use_data(
  data_example_intelligibility_by_length,
  overwrite = TRUE
)
