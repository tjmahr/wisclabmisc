str_unnumber <- function(string) {
  stringr::str_remove_all(string, "\\d+$")
}

blanks <- c("sil", "sp", "SIL", "", "SPN", "<UNK>", "spn")

# To remove many of the pronunciation dictionary differences, we can normalize
# vowels. This makes it easier to join the manual intervals to the automatic
# intervals.
str_normalize_vowels <- function(xs) {
  xs %>%
    stringr::str_replace_all("^vowel$", "vowel") %>%
    stringr::str_replace_all("^AH$", "vowel") %>%
    stringr::str_replace_all("^IH$", "vowel") %>%
    stringr::str_replace_all("^AA$", "vowel") %>%
    stringr::str_replace_all("^IY$", "vowel") %>%
    stringr::str_replace_all("^AE$", "vowel") %>%
    stringr::str_replace_all("^EH$", "vowel") %>%
    stringr::str_replace_all("^EY$", "vowel") %>%
    stringr::str_replace_all("^ER$", "vowel") %>%
    stringr::str_replace_all("^UW$", "vowel") %>%
    stringr::str_replace_all("^UH$", "vowel") %>%
    stringr::str_replace_all("^OY$", "vowel") %>%
    stringr::str_replace_all("^UH$", "vowel") %>%
    stringr::str_replace_all("^AO$", "vowel") %>%
    stringr::str_replace_all("^AW$", "vowel") %>%
    stringr::str_replace_all("^AY$", "vowel") %>%
    stringr::str_replace_all("^OW$", "vowel")
}

set_phone_class <- function(xs) {
  stops <- c("B", "D", "G", "K", "T", "P")
  nasals <- c("NG", "N", "M")
  fricatives <- c("SH", "ZH", "TH", "DH",  "S", "Z", "F", "V", "HH")

  class <- ifelse(xs %in% stops, "plosives", "others")
  class <- ifelse(xs %in% fricatives, "fricatives", class)
  class <- ifelse(xs == "vowel", "vowels", class)
  class
}


arpabet_vowel_cheatsheet <- c(
  "AH" = "AH (hud)",
  "IY" = "IY (heed)",
  "IH" = "IH (hid)",
  "AE" = "AE (had)",
  "EY" = "EY (hay)",
  "ER" = "ER (herd)",
  "AA" = "AA (hot)",
  "UW" = "UW (hoo)",
  "AY" = "AY (hide)",
  "OW" = "OW (hoe)",
  "AO" = "AO (hawk)",
  "AW" = "AW (how)",
  "EH" = "EH (head)",
  "UH" = "UH (hood)",
  "OY" = "OY (hoy)"
)


wiscbet_to_ipa <- function(...) {
  xs <- c(...)
  rules <- c(
    "i"  = "i",       # beat
    "I"  = "\u026a",  # bit
    "eI" = "e",       # bait
    "E"  = "\u025b",  # bet
    "ae" = "\u00e6",  # bat
    "^"  = "\u028c",  # but
    "3^" = "\u025d",  # Bert
    "4"  = "\u0259",  # comma: unstressed, neutral vowel
    "4^" = "\u025a",  # letter: unstressed, neutral r-colored vowel
    "@"  = "\u0251",  # bot
    "oU" = "o",       # boat
    "c"  = "\u0254",  # bought
    "u"  = "u",       # boot
    "U"  = "\u028a",  # book
    # "3"  = "\u025c",  # [unclear]
    # "a"  = "a",       # [unclear]
    # "D"  = "\u0252",  # [unclear]
    "@I" = "a\u026a", # bite
    "@U" = "a\u028a", # bout
    "cI" = "\u0254\u026a",  # boyd

    "p" = "p", "b" = "b", "m" = "m",
    "t" = "t", "d" = "d", "n" = "n",
    "k" = "k", "g" = "g", "ng"= "\u014b",
    "tsh" = "t\u0283", "dzh" = "d\u0292", # cheap, jeep

    "f"  = "f", "v"  = "v",
    "th" = "\u03b8", "dh" = "\u00f0", # mouth, mouthe
    "s"  = "s", "z"  = "z",
    "sh" = "\u0283", "zh" = "\u0292", # bash, beige
    "h" = "h",

    "j" = "j", "w" = "w",
    "r" = "r", "l" = "l",
    "." = ".", " " = " "
  )
  unname(rules[xs])
}



wiscbet <- local({
  wb_vowels <- c(
    "i",
    "I",
    "e",
    "E",
    "ae",
    "3",
    "3^",
    "4",

    "4^",
    "^",
    "a",
    "u",
    "U",
    "o",
    "c",
    "D",

    "@",
    "@I",
    "@U",
    "eI",
    "oU",
    "cI"
  )

  wb_early8 <- c(
    "p",
    "b",
    "m",
    "d",
    "n",
    "j",
    "w",
    "h"
  )

  wb_middle8 <- c(
    "f",
    "v",
    "t",
    "tsh",
    "dzh",
    "k",
    "g",
    "ng"
  )

  wb_late8 <- c(
    "th",
    "dh",
    "s",
    "z",
    "sh",
    "zh",
    "r",
    "l"
  )

  wb_consonants <- c(
    wb_early8,
    wb_middle8,
    wb_late8
  )

  list(
    vowels = wb_vowels,
    consonants = wb_consonants,
    early8 = wb_early8,
    middle8 = wb_middle8,
    late8 = wb_late8
  )
})


