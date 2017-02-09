test_that('key_from_file reads from a file as expected', {
  original_key <- Sys.getenv("QUALTRICS_KEY")
  fname <- tempfile()
  cat("key_value", file = fname, fill = TRUE)

  expect_silent(value <- key_from_file(fname))
  expect_equal(value, "key_value")
  expect_message(key_from_file(fname, verbose = TRUE), fname)

  cat("", file = fname, fill = TRUE)
  expect_error(key_from_file(fname), "Couldn't find key")

  cat("line one\nline_two", file = fname, fill = TRUE)
  expect_equal(key_from_file(fname), "line one")

  Sys.setenv("QUALTRICS_KEY" = original_key)
})

