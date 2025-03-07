data_features_vowels <- readr::read_csv(
  "data-raw/vowels.csv",
  col_types = "c"
) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    backness_fct = factor(backness, c("front", "central", "back")),
    height_fct = factor(height, c("high", "mid-high", "mid-low", "low"))
  ) |>
  dplyr::relocate(backness_fct, .after = backness) |>
  dplyr::relocate(height_fct, .after = height)

place_levels <- c(
    "labial",
    "labiodental",
    "dental",
    "alveolar",
    "postalveolar",
    "palatal",
    "velar",
    # omitting "labiovelar",
    "glottal"
)

data_features_consonants <- readr::read_csv(
  "data-raw/consonants.csv",
  col_types = "c"
) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    place_fct = factor(place, place_levels)
  ) |>
  dplyr::relocate(place_fct, .after = place)

usethis::use_data(
  data_features_vowels,
  overwrite = TRUE
)

usethis::use_data(
  data_features_consonants,
  overwrite = TRUE
)
