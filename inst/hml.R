library(tidyverse)
x <- readr::read_table("C:/Users/mahr/Desktop/HML.txt")
y <- readr::read_table("C:/Users/mahr/Desktop/HML(1).txt")
z <- readr::read_table("C:/Users/mahr/Desktop/Ch02.Textfile2.txt")
problems(x)
problems(y)
problems(z)

waldo::compare(x, y)
waldo::compare(x, z)

x |>
  filter(stringr::str_detect(HMLbet, "h")) |>
  arrange(desc(frequency))

hmlbet_to_ipa <- c(
  x = "ə", a = "ɑ", r = "r", d = "d", v = "v",
  k = "k", b = "b", `@` = "æ", s = "s", f = "f",
  t = "t", l = "l", o = "oʊ", n = "n", i = "i",
  e = "eɪ", S = "ʃ", w = "w", I = "ɪ",
  m = "m", L = "l", `^` = "ʌ", E = "ɛ",
  h = "h", Y = "aɪ", J = "dʒ", U = "ʊ", z = "z",
  u = "u", g = "g", G = "ŋ", W = "aʊ", c = "ɔ",
  p = "p", `T` = "θ", M = "m",
  y = "j",
  C = "tʃ", N = "n", Z = "ʒ", O = "ɔɪ", D = "ð",
  # these produced mismatches with aggregation by Beckman
  `|` = "ə",  # `|` = "ɪ",
  X = "ɚ",
  R = "ɝ"
)

# every hmlbet char that isn't its own IPA counterpart
hmlbet_to_ipa[hmlbet_to_ipa != names(hmlbet_to_ipa)]

# coverage of english IPA
ipa <- c(
  wisclabmisc::data_features_vowels$phone,
  wisclabmisc::data_features_consonants$phone
)

ipa %in% hmlbet_to_ipa
hmlbet_to_ipa %in% ipa

chars <- x$HMLbet |>
  strsplit("") |>
  unlist()

# Precomputed counts I found for a course taught by Beckman and others
# https://kb.osu.edu/items/6b1379d3-e15d-53e4-99b6-85f2bb09b3af
z <- 'ipa count
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

zz <- readr::read_delim(z)


data_freq <- x |>
  select(HMLbet, frequency) |>
  mutate(
    sounds = strsplit(HMLbet, "")
  ) |>
  tidyr::unnest(sounds) |>
  mutate(
    ipa = hmlbet_to_ipa[sounds],
  ) |>
  group_by(sounds) |>
  summarise(
    ipa = unique(ipa),
    n_instances = n(),
    n_words = n_distinct(HMLbet),
    frequency = sum(frequency)
  ) |>
  group_by(ipa) |>
  mutate(
    ipa_frequency = sum(frequency)
  ) |>
  ungroup() |>
  arrange(ipa, sounds)

zz$ipa[! zz$ipa %in% data_freq$ipa]

# it looks like they treated /h/ as schwa?
# i don't buy treating /ɚ/ as /r/
data_freq |>
  left_join(zz) |>
  mutate(
    diff = ipa_frequency - count
  ) |>
  print(n = Inf)




# checking that /|/ means i-bar vowel
x |>
  filter(str_detect(orthography, "basis")) |>
  arrange(desc(familiarity))

x |>
  filter(orthography %in% c("nothing", "limit", "message")) |>
  arrange(desc(familiarity))
