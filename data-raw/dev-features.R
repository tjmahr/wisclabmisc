library(tidyverse)

make_crowe_mcleod <- function() {
  # I pasted the table into NotePad++ and did some find and replace to get.
  # - change 1) I dropped the voiceless /ʍ/ row
  # - change 2) Used /r/ instead of /ɹ/ for the English approximant sound
  # - change 3) Used /g/ instead of /ɡ/
  # - change 4) Used /tʃ, dʒ/ instead of single-character versions.
  data_crowe_mcleod <- readr::read_csv(
  "phone,M_50,SD_50,Range_50,N_50,M_75,SD_75,Range_75,N_75,M_90,SD_90,Range_90,N_90
  p,30.60,7.18,18–36,10,32.73,5.61,24–36,11,33.25,6.94,24–48,12
  b,30.60,7.18,18–36,10,32.73,5.61,24–36,11,31.38,7.81,24–48,13
  t,31.20,6.20,24–36,10,33.82,7.24,24–48,11,38.54,9.19,24–60,13
  d,30.60,7.18,18–36,10,33.09,5.09,24–36,11,35.69,6.68,24–48,13
  k,31.20,6.20,24–36,10,33.82,4.85,24–36,11,37.69,7.30,24–48,13
  g,31.20,6.20,24–36,10,33.82,4.85,24–36,11,36.77,6.61,24–48,13
  m,30.60,7.18,18–36,10,32.73,5.61,24–36,11,33.23,6.66,24–48,13
  n,30.60,7.18,18–36,10,32.73,5.61,24–36,11,33.08,7.42,24–48,13
  ŋ,30.00,6.41,24–36,8,36.67,12.17,24–66,9,40.30,10.75,24–55,10
  f,31.20,6.20,24–36,10,33.82,4.85,24–36,11,38.31,6.26,24–48,13
  v,32.80,5.27,24–36,10,42.73,11.64,30–72,11,50.83,10.77,36–66,12
  θ,46.00,7.66,36–60,10,64.20,4.94,60–72,10,77.00,7.44,72–96,10
  ð,41.80,4.94,36–48,10,56.73,7.28,48–72,11,69.00,11.33,54–96,12
  s,32.40,5.80,24–36,10,38.55,10.00,24–60,11,51.33,16.32,24–84,12
  z,33.40,5.97,24–42,10,44.40,17.02,24–84,10,56.82,14.28,30–84,11
  ʃ,32.40,5.80,24–36,10,41.27,10.21,24–60,11,55.00,10.50,36–72,12
  ʒ,37.00,8.25,28–48,4,54.00,16.54,36–84,6,70.67,12.22,60–84,3
  h,30.60,7.18,18–36,10,32.73,5.61,24–36,11,35.00,6.95,24–48,13
  r,35.40,7.18,24–48,10,47.64,13.02,24–66,11,66.58,18.62,30–96,12
  j,33.00,5.10,24–36,10,39.60,7.59,24–48,10,45.77,10.96,30–60,13
  l,33.20,5.01,24–36,10,40.91,7.97,24–48,11,53.75,10.43,24–60,12
  w,30.60,7.18,18–36,10,32.73,5.61,24–36,11,35.23,6.76,24–48,13
  tʃ,34.20,4.05,24–36,10,41.64,8.71,24–54,11,53.50,10.69,36–72,12
  dʒ,34.20,4.05,24–36,10,41.27,8.68,24–54,11,51.00,11.82,36–72,13
"
  ) |>
    janitor::clean_names() |>
    mutate(phone = str_trim(phone)) |>
    separate(range_90, c("min_90", "max_90"), sep = "\\D") |>
    separate(range_50, c("min_50", "max_50"), sep = "\\D") |>
    separate(range_75, c("min_75", "max_75"), sep = "\\D") |>
    mutate(
      across(c(starts_with("min"), starts_with("max")), as.numeric)
    ) |>
    pivot_longer(
      -phone,
      names_to = c("stat", "criterion"),
      names_sep = "_",
      values_to = "value"
    ) |>
    pivot_wider(names_from = stat, values_from = value) |>
    rename(
      age_mean = m,
      age_sd = sd,
      age_min = min,
      age_max = max,
      num_studies = n
    ) |>
    # - change 5) rounded mean and sd to 1 decimal
    mutate(
      age_mean = round(age_mean, 1),
      age_sd = round(age_sd, 1)
    )

  data_crowe_mcleod2 <- data_crowe_mcleod |>
    filter(criterion == 90) |>
    mutate(
      stage = case_when(
        age_mean < 4 * 12 ~ "early",
        age_mean < 5 * 12 ~ "middle",
        age_mean < 7 * 12 ~ "late",
      )
    ) |>
    select(-criterion)

  names(data_crowe_mcleod2) <- names(data_crowe_mcleod2) |>
    stringr::str_replace("age_", "cm2020_90_age_") |>
    stringr::str_replace("stage", "cm2020_90_stage") |>
    stringr::str_replace("num_studies", "cm2020_90_num_studies")

  data_crowe_mcleod2
}

make_shriberg_eights <- function() {
  s8_e <- c("m", "b", "j", "n", "w", "d",  "p",  "h")
  s8_m <- c("t", "ŋ", "k", "g", "f", "v", "tʃ", "dʒ")
  s8_l <- c("ʃ", "θ", "s", "z", "ð", "l",  "r",  "ʒ")

  data_s93 <- data.frame(
    phone = c(s8_e, s8_m, s8_l),
    s93_eights = c(rep("early", 8), rep("middle", 8), rep("late", 8))
  )
}

make_complexity_scales <- function() {
  vowels <- local({
    v <- c(
      "AH" = 1, "AA" = 1,
      "IY" = 2, "UW" = 2, "OW" = 2,
      "AY" = 3, "AO" = 3, "AW" = 3, "EH" = 3, "OY" = 3,
      "IH" = 4, "AE" = 4, "EY" = 4, "UH" = 4, "ER" = 5
    )
    data.frame(cmubet = names(v), complexity = unname(v))
  })

  consonants <- local({
    v <- c(
       "P" = 3,  "M" = 3,  "N" = 3,  "W" = 3, "HH" = 3,
       "B" = 4,  "D" = 4,  "K" = 4,  "G" = 4,  "Y" = 4,  "F" = 4,
       "T" = 5, "NG" = 5,  "R" = 5,  "L" = 5,
       "S" = 6,  "Z" = 6, "SH" = 6, "ZH" = 6,  "V" = 6, "TH" = 6, "DH" = 6,
      "CH" = 6, "JH" = 6
    )
    data.frame(cmubet = names(v), complexity = unname(v))
  })

  consonants2 <- consonants
  consonants2$complexity <- consonants2$complexity - 2
}

data_s93 <- make_shriberg_eights()
data_crowe_mcleod <- make_crowe_mcleod()

data_acq_consonants <- readr::read_csv("data-raw/consonants.csv") |>
  select(phone, cmubet, wiscbet) |>
  right_join(data_crowe_mcleod, by = "phone") |>
  left_join(data_s93, by = "phone") |>
  print(n = Inf)



usethis::use_data(
  data_acq_consonants,
  overwrite = TRUE
)

