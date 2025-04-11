# We retrieve two corpora from a private Github repo.

retrieve_private_file <- function(filepath) {
  repo <- "tjmahr/backroom-corpora"
  branch <- "main"

  api_url <- sprintf(
    "https://api.github.com/repos/%s/contents/%s?ref=%s",
    repo, filepath, branch
  )

  response <- httr::GET(
    api_url, httr::add_headers(Authorization = paste("token", gh::gh_token()))
  )

  if (httr::status_code(response) != 200) {
    stop("Failed to get file. Status code: ", httr::status_code(response))
  }

  # GitHub API returns JSON with the file $contents base64-encoded
  # https://docs.github.com/en/rest/repos/contents?apiVersion=2022-11-28
  httr::content(response, as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON() |>
    getElement("content") |>
    # Returning the raw bytes
    base64enc::base64decode()
}

download_missing_private_file <- function(local_path, remote_path) {
  if (!file.exists(local_path)) {
    message("File not found locally. Downloading from GitHub.")
    bytes <- retrieve_private_file(remote_path)
    writeBin(bytes, local_path)
  }

  invisible(local_path)
}

self_name <- function(x) stats::setNames(x, x)



library(dplyr)

# We retrieve two corpora from a private Github repo.
download_missing_private_file("data-raw/moe-word-freq.wp", "moe-word-freq.wp")
download_missing_private_file("data-raw/hml-word-freq.txt", "hml-word-freq.txt")
usethis::use_git_ignore("moe-word-freq.wp", "data-raw")
usethis::use_git_ignore("hml-word-freq.txt", "data-raw")

# American English IPA characters
ipa_phones <- c(
  wisclabmisc::data_features_vowels$phone,
  wisclabmisc::data_features_consonants$phone
)

data_mhr <- readr::read_delim("data-raw/moe-word-freq.wp")

# MHR uses numbered vowels, inherited presumably from the CMU dictionary.
# Patch any entries with unnumbered vowels.
data_mhr$pron <- data_mhr$pron |>
  # BABYSITTED, EXTINGUISH, FIRECRACKER
  stringr::str_replace("^be1bi0sI2tx$", "be1bi0sI2tx0d") |>
  stringr::str_replace("^I0kstI1NgwI$", "I0kstI1NgwI0S") |>
  stringr::str_replace("^fY1R0kr&2kR$", "fY1R0kr&2kR0") |>
  # REVERSE, PRACTICALLY, ALTOGETHER,
  stringr::str_replace("^rI0vRs$", "rI0vR1s") |>
  stringr::str_replace("^pr&1ktx0kli$", "pr&1ktx0kli0") |>
  stringr::str_replace("^O2ltx0gE1DR$", "O2ltx0gE1DR0") |>
  # FOUR-NINETY-FIVE (the interstate?), HOKEY-POKEY
  stringr::str_replace("^fO2rnY2ntifY1v$", "fO2rnY2nti0fY1v") |>
  stringr::str_replace("^ho1ki0po2ki$", "ho1ki0po2ki0")

# Patch any other dubious pronunciations
data_mhr[data_mhr$orth == "WAS", "pron"] <- "wx0z"
data_mhr[data_mhr$orth == "BECAUSE", "pron"] <- "bI0kx1z"

# Check for any unnumbered vowels
unnumbered_vowel <- "[&aceEiIoORuUWxY](\\D|$)"
data_mhr |>
  filter(stringr::str_detect(pron, unnumbered_vowel)) |>
  arrange(desc(freq)) |>
  print(n = 20)


# Every character that occurs before a number (vowels)
data_mhr$pron |>
  stringr::str_extract_all(".\\d") |>
  unlist() |>
  stringr::str_remove_all("\\d") |>
  table()

phone_tokens <- data_mhr$pron |>
  stringr::str_extract_all("\\D\\d|\\D") |>
  unlist()

phone_tokens |> table() |> sort()

# Some code for check which characters appear in which words
char_to_check <- "Y1"
data_mhr |>
  filter(stringr::str_detect(pron, char_to_check)) |>
  arrange(desc(freq)) |>
  print(n = 20)

# Create the mapping between MHR-bet (names) and IPA (values).
# I initialize the vector with `dput(phones)` and then did data entry.
phones <- phone_tokens |>
  unique() |>
  self_name() |>
  sort()

mhr_to_ipa <- c(
  # front
  i0 = "i", i1 = "i", i2 = "i",
  I0 = "ɪ", I1 = "ɪ", I2 = "ɪ",
  e0 = "eɪ", e1 = "eɪ", e2 = "eɪ",
  E0 = "ɛ", E1 = "ɛ", E2 = "ɛ",
  `&0` = "æ", `&1` = "æ", `&2` = "æ",
  # back
  u0 = "u", u1 = "u", u2 = "u",
  U0 = "ʊ", U1 = "ʊ", U2 = "ʊ",
  o0 = "oʊ", o1 = "oʊ", o2 = "oʊ",
  O0 = "ɔ", O1 = "ɔ", O2 = "ɔ",
  a0 = "ɑ", a1 = "ɑ", a2 = "ɑ",
  # etc
  c0 = "ɔɪ", c1 = "ɔɪ", c2 = "ɔɪ",
  W0 = "aʊ", W1 = "aʊ", W2 = "aʊ",
  Y0 = "aɪ", Y1 = "aɪ", Y2 = "aɪ",
  # Making a decision here to treat 0-stressed versions as
  # unstressed versions of the vowels.
  x0 = "ə", x1 = "ʌ", x2 = "ʌ",
  R0 = "ɚ", R1 = "ɝ", R2 = "ɝ",
  # consonants
  b = "b", C = "tʃ",
  d = "d", D = "ð",
  f = "f", g = "g", h = "h",
  m = "m", n = "n", N = "ŋ",
  j = "j", J = "dʒ", k = "k", l = "l",
  p = "p", r = "r",
  t = "t", `T` = "θ",
  v = "v", w = "w",
  s = "s", S = "ʃ",
  z = "z", Z = "ʒ"
)

# Which symbols did/didn't need adjustment
mhr_to_ipa[mhr_to_ipa != names(mhr_to_ipa)]
mhr_to_ipa[mhr_to_ipa == names(mhr_to_ipa)]

# Do we cover our English IPA inventory?
setdiff(mhr_to_ipa, ipa_phones)
setdiff(ipa_phones, mhr_to_ipa)

# Compute frequency-weighted phoneme counts
data_mhr_counts <- data_mhr |>
  # Get the phones in each word
  group_by(orth) |>
  mutate(
    sounds = stringr::str_extract_all(pron, "\\D\\d|\\D")
  ) |>
  tidyr::unnest(sounds) |>
  mutate(
    ipa = mhr_to_ipa[sounds],
  ) |>
  # Add up the frequencies for each MHR-bet character
  group_by(sounds) |>
  summarise(
    ipa = unique(ipa),
    n_instances = n(),
    n_words = n_distinct(pron),
    frequency = sum(freq)
  ) |>
  # Add up the frequencies for each IPA character
  group_by(ipa) |>
  mutate(
    ipa_frequency = sum(frequency)
  ) |>
  ungroup() |>
  arrange(ipa, sounds)

# Double check our counts using the prepared counts from the course's website
data_prepared_mhr_counts <- local({
  mhr_counts_string <- '
ipa count
"i" 28537
"ɪ" 43999
"eɪ" 16679
"ɛ" 31322
"æ" 40290
"ɑ" 20601
"ɔ" 13021
"ʊ" 4288
"oʊ" 17330
"u" 19188
"ʌ" 21559
"ɝ" 4244
"aɪ" 23807
"aʊ" 7377
"ɔɪ" 744
"ə" 46360
"p" 14851
"b" 18027
"t" 69108
"d" 47837
"tʃ" 3149
"dʒ" 3015
"k" 25462
"g" 15789
"m" 25799
"n" 68331
"ŋ" 9102
"f" 10731
"v" 8753
"θ" 4337
"ð" 35995
"s" 33752
"z" 24027
"ʃ" 4843
"ʒ" 100
"w" 25171
"r" 40193
"l" 29343
"j" 8544
'
  readr::read_delim(mhr_counts_string, col_types = "ci")
})

data_mhr_count_differences <- data_mhr_counts |>
  full_join(data_prepared_mhr_counts, by = "ipa") |>
  mutate(
    diff = ipa_frequency - count
  )

# Looks like
#   1) they lost /h/,
#   2) treated /ɚ/ as /ə/ + /r/,
#   3) plus we lost 3275 /ɑ/ tokens when we fixed WAS
#   4) and lost 1017 /ɔ/ tokens when we fixed BECAUSE
data_mhr_count_differences |>
  filter(diff != 0 | is.na(diff)) |>
  print(n = Inf)


# When we remove /h/, they still have 10000 more because they double count
# sounds from /ɚ/
data_mhr_count_differences |>
  filter(ipa != "h") |>
  pull(frequency) |>
  sum()
data_prepared_mhr_counts$count |> sum()

#
# library(ggplot2)
#
# ggplot(both) +
#   aes(cm2020_90_age_mean, y = ipa_frequency) +
#   # geom_point() +
#   geom_text(aes(label = phone)) +
#   scale_y_log10()
#
#
# lm(cm2020_90_age_mean ~ log10(ipa_frequency), both) |> summary()
#


## HML frequencies -----

data_hml <- readr::read_delim("data-raw/hml-word-freq.txt")
# one of the rows has two extra columns so reader complains about them

# Patch away syllablic Cs
data_hml$HMLbet <- data_hml$HMLbet |>
  stringr::str_replace_all("L", "xl") |>
  stringr::str_replace_all("M", "xm") |>
  stringr::str_replace_all("N", "xn")

# Prepare the mapping from HMLBet (names) to IPA (values)
hmlbet_to_ipa <- c(
  x = "ə", `^` = "ʌ",
  E = "ɛ", `@` = "æ",
  a = "ɑ",
  r = "r", d = "d", v = "v",
  k = "k", b = "b",
  s = "s", f = "f",
  t = "t", l = "l", o = "oʊ", n = "n", i = "i",
  e = "eɪ", S = "ʃ", w = "w", I = "ɪ",
  m = "m",
  # syllablic C handled above
  # L = "l", M = "m", N = "n",
  h = "h", Y = "aɪ", J = "dʒ", U = "ʊ", z = "z",
  u = "u", g = "g", G = "ŋ", W = "aʊ", c = "ɔ",
  p = "p", `T` = "θ",
  y = "j",
  C = "tʃ", Z = "ʒ", O = "ɔɪ", D = "ð",
  R = "ɝ",
  # these choices by produced mismatches with the aggregation in the course
  `|` = "ɪ",
  X = "ɚ"
)


# Which symbols did/didn't need adjustment
hmlbet_to_ipa[hmlbet_to_ipa != names(hmlbet_to_ipa)]
hmlbet_to_ipa[hmlbet_to_ipa == names(hmlbet_to_ipa)]

# Do we cover our English IPA inventory?
setdiff(hmlbet_to_ipa, ipa_phones)
setdiff(ipa_phones, hmlbet_to_ipa)

# Compute frequency-weighted phoneme counts
data_hml_counts <- data_hml |>
  # Get the phones in each word
  group_by(orthography) |>
  mutate(
    sounds = strsplit(HMLbet, "")
  ) |>
  tidyr::unnest(sounds) |>
  mutate(
    ipa = hmlbet_to_ipa[sounds],
  ) |>
  # Add up the frequencies for each MHR-bet character
  group_by(sounds) |>
  summarise(
    ipa = unique(ipa),
    n_instances = n(),
    n_words = n_distinct(HMLbet),
    frequency = sum(frequency)
  ) |>
  # Add up the frequencies for each IPA character
  group_by(ipa) |>
  mutate(
    ipa_frequency = sum(frequency)
  ) |>
  ungroup() |>
  arrange(ipa, sounds)

# Double check our counts using the prepared counts from the course's website
data_prepared_hml_counts <- local({
  hml_counts_string <- '
ipa count
"i" 82430
"ɪ" 154298
"eɪ" 38966
"ɛ" 73107
"æ" 106838
"ɑ" 36589
"ɔ" 26110
"ʊ" 13557
"oʊ" 61954
"u" 57320
"ʌ" 44555
"ɝ" 17048
"aɪ" 39275
"aʊ" 16079
"ɔɪ" 2140
"ə" 313590
"p" 50694
"b" 51831
"t" 188536
"d" 102205
"tʃ" 17147
"dʒ" 13220
"k" 73250
"g" 19422
"m" 74856
"n" 204939
"ŋ" 13692
"f" 51526
"v" 66490
"θ" 18300
"ð" 108602
"s" 114733
"z" 54454
"ʃ" 21756
"ʒ" 1488
"w" 60251
"r" 163514
"l" 98287
"j" 16510
'
  readr::read_delim(hml_counts_string, col_types = "ci")
})


data_hml_count_differences <- data_hml_counts |>
  full_join(data_prepared_hml_counts, by = "ipa") |>
  mutate(
    diff = ipa_frequency - count
  )

data_hml_count_differences |>
  arrange(ipa, sounds) |>
  print(n = Inf)

data_hml |>
  filter(stringr::str_detect(HMLbet, "[|]")) |>
  arrange(desc(frequency))

# Looks like they
#   1) ignored /h/
#   2) did not treat M as /əm/
#   3) treated /ɚ/ as /ə/ + /r/
#   3) treated "|" as /ə/
data_hml_count_differences |>
  filter(diff != 0 | is.na(diff)) |>
  print(n = Inf)


ONE_MILLION <- 1e6
data_hml_counts2 <- data_hml_counts |>
  distinct(ipa, ipa_frequency) |>
  mutate(
    freq_per_million = (ipa_frequency / sum(ipa_frequency)) * ONE_MILLION,
    log10_freq_per_million = log10(freq_per_million)
  ) |>
  select(
    phone = ipa,
    hml84_frequency = ipa_frequency,
    hml84_log10fpm = log10_freq_per_million,
  )

data_mhr_counts2 <- data_mhr_counts |>
  distinct(ipa, ipa_frequency) |>
  mutate(
    freq_per_million = (ipa_frequency / sum(ipa_frequency)) * ONE_MILLION,
    log10_freq_per_million = log10(freq_per_million)
  ) |>
  select(
    phone = ipa,
    mhr82_frequency = ipa_frequency,
    mhr82_log10fpm = log10_freq_per_million,
  )

data_freq <- data_hml_counts2 |>
  left_join(data_mhr_counts2, by = "phone")

library(ggplot2)
ggplot(data_freq) +
  aes(x = mhr82_log10fpm, y = hml84_log10fpm ) +
  geom_text(aes(label = phone)) +
  geom_abline() +
  coord_fixed() +
  expand_limits(x = 2, y = 2)

readr::write_csv(data_freq, "data-raw/phone-freqs.csv")
