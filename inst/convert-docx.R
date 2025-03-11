# Convert .docx versions of supplemental materials files to .md

rmarkdown::pandoc_convert(
  input = "vignettes/articles/docx/sm-mahr-2024-intel-rate.docx",
  to = "markdown+fenced_code_blocks",
  output = "vignettes/articles/docx/sm-mahr-2024-intel-rate.md",
  # make the sure code blocks are explicitly labeled ` r`
  options = c(
    "--lua-filter=vignettes/articles/docx/r-codeblock.lua"
  ),
  wd = "."
)
