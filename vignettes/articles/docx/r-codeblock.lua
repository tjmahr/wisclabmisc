function CodeBlock(el)
  -- If code block has no classes, set 'r' as the language
  if #el.classes == 0 then
    el.classes = { 'r' }
  end
  return el
end
