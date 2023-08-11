test_that("str_extract_tocs_item", {

  x <- c(
    "XXv17_SLOW_s4T06.TextGrid",
    "XXv14_SLOW_s5T04.TextGrid",
    "XXv16s6T06.TextGrid",
    "XX(b)v16s7T03.TextGrid",
    "XXv15_SLOW_s2T01.TextGrid",
    "XXv18wT11.wav",
    "XXv16s7T06.lab",
    "XXv15s5T06.lab",
    "XXv13s3T10.WAV",
    "XXv16S2T09.wav",
    "s2T01",
    "invalid-file.txt"
  )

  expected <- c(
    "S4T06",
    "S5T04",
    "S6T06",
    "S7T03",
    "S2T01",
    "WT11",
    "S7T06",
    "S5T06",
    "S3T10",
    "S2T09",
    "S2T01",
    NA_character_
  )

  expect_equal(
    tocs_item(x),
    expected
  )
})
