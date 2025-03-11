# Convert .docx versions of supplemental materials files to .md

convert_one <- function(input_path) {
  output_path <- input_path |>
    tools::file_path_sans_ext() |>
    paste0(".md") |>
    basename()

  rmarkdown::pandoc_convert(
    input = file.path("docx", basename(input_path)),
    to = "markdown+fenced_code_blocks",
    output = basename(output_path),
    options = c(
      # make the sure code blocks are explicitly labeled ` r`
      "--lua-filter=docx/r-codeblock.lua",
      "--extract-media=.",
      # add calls to xfun::base64_uri() to embed images until
      # https://github.com/r-lib/pkgdown/pull/2811 is fixed in the
      # CRAN version
      "--lua-filter=docx/add-base64-calls.lua"
    ),
    wd = "vignettes/articles/",
    verbose = TRUE
  )
}

input_path <- "vignettes/articles/docx/sm-2022-long-vss.docx"
convert_one("vignettes/articles/docx/sm-2025-mahr-intel-rate.docx")
convert_one("vignettes/articles/docx/sm-2022-long-vss.docx")
# pkgdown::build_article("articles/sm-2022-long-vss")
