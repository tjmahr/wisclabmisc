test_that("audit_wrap creates an audit with data and empty notes", {
  x <- audit_wrap(letters)

  expect_s3_class(x, "audit")
  expect_true(is.list(x))
  expect_named(x, c("data", "notes"))

  expect_identical(x$data, letters)
  expect_identical(x$notes, list())
})

test_that("audit_unwrap returns the underlying data", {
  x <- audit_wrap(letters)
  expect_identical(audit_unwrap(x), letters)
})

test_that("audit_peek logs a note and does not modify data", {
  x <- audit_wrap(letters)
  y <- audit_peek(x, toupper)

  expect_s3_class(y, "audit")
  expect_identical(y$data, letters)

  expect_length(y$notes, 1L)
  expect_named(y$notes)

  nm <- names(y$notes)[[1]]
  expect_match(nm, "^toupper\\(x.*\\)$")

  note <- y$notes[[1]]
  expect_s3_class(note, "audit_note")
  expect_identical(note$type, "peek")
  expect_identical(note$label, nm)
  expect_identical(note$result, toupper(letters))
})

test_that("audit_poke logs a note and overwrites data", {
  x <- audit_wrap(letters)
  y <- audit_poke(x, toupper)

  expect_s3_class(y, "audit")
  expect_identical(y$data, toupper(letters))

  expect_length(y$notes, 1L)
  nm <- names(y$notes)[[1]]
  expect_match(nm, "^toupper\\(x.*\\)$")

  note <- y$notes[[1]]
  expect_identical(note$type, "poke")
  expect_identical(note$result, toupper(letters))
})

test_that("peek/poke accumulate notes in order and allow ... args", {
  x <- audit_wrap(letters) |>
    audit_peek(rev) |>
    audit_poke(toupper) |>
    audit_peek(getElement, 1)

  expect_length(x$notes, 3L)

  expect_identical(x$notes[[1]]$type, "peek")
  expect_identical(x$notes[[2]]$type, "poke")
  expect_identical(x$notes[[3]]$type, "peek")

  # After poke(toupper), data is uppercased; getElement(1) should reflect that
  expect_identical(x$data, toupper(letters))
  expect_identical(x$notes[[3]]$result, "A")
})

test_that("anonymous functions are labeled with <unnamed-function-k>", {
  x <- audit_wrap(letters) |>
    audit_peek(\(x) x[x %in% c("a", "e", "i", "o", "u")])

  expect_length(x$notes, 1L)
  nm <- names(x$notes)[[1]]

  # Label should be rewritten to <unnamed-function-1>(...)
  expect_match(nm, "^`<unnamed-function-1>`\\(x.*\\)$")
  expect_identical(x$notes[[1]]$label, nm)
  expect_identical(x$notes[[1]]$result, c("a", "e", "i", "o", "u"))
})

test_that("note naming uses the current note count for unnamed functions", {
  x <- audit_wrap(letters) |>
    audit_peek(rev) |>
    audit_peek(\(x) x[1:2])

  expect_length(x$notes, 2L)
  nm2 <- names(x$notes)[[2]]
  expect_match(nm2, "^`<unnamed-function-2>`\\(x.*\\)$")
})

test_that("print.audit prints a header and notes section and returns the object", {
  x <- audit_wrap(letters) |>
    audit_peek(rev) |>
    audit_poke(toupper) |>
    audit_peek(getElement, 1)

  out <- capture.output(ret <- print(x))

  # print returns x (your method returns x explicitly)
  expect_s3_class(ret, "audit")
  expect_identical(ret, x)

  # basic structure in output
  expect_true(any(grepl("^<audit> object$", out)))
  expect_true(any(grepl("^notes:$", out)))

  # should mention peek/poke in the notes names
  expect_true(any(grepl("\\[peek\\]", out)))
  expect_true(any(grepl("\\[poke\\]", out)))
})

