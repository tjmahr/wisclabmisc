function Image(el)
  -- Escape single quotes in the path, just in case
  local escaped_src = el.src:gsub("'", "\\'")
  -- Preserve alt text (el.caption is a list of inline elements)
  local alt_text = pandoc.utils.stringify(el.caption)
  -- Build the final Markdown image tag with inline R code as source
  local markdown_img = string.format(
    "![%s](`r xfun::base64_uri('%s')`)",
    alt_text,
    escaped_src
  )
  -- Return a raw inline element containing Markdown
  return pandoc.RawInline('markdown', markdown_img)
end
