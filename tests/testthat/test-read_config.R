test_that("read_config reads from a file as expected", {
  original_token <- Sys.getenv("QUALTRICS_TOKEN")
  fname <- tempfile()
  cat("{\"QUALTRICS_TOKEN\": \"token_value\"}", file = fname, fill = TRUE)

  expect_silent(value <- read_config(fname))
  expect_equal(names(value), "QUALTRICS_TOKEN")
  expect_message(read_config(fname, verbose = TRUE), "Set from file")

  Sys.setenv("QUALTRICS_TOKEN" = original_token)
})

test_that("read_config sets environment variables", {
  fname <- tempfile()
  cat("{\"LINE1\": \"line1_value\", \"LINE2\": \"line2_value\"}",
    file = fname, fill = TRUE)
  expect_equal(read_config(fname), list(LINE1 = "line1_value", LINE2 =
      "line2_value"))
  expect_equal(Sys.getenv("LINE1"), "line1_value")
  expect_equal(Sys.getenv("LINE2"), "line2_value")
})

test_that("read_config stops if keys not found", {
  fname <- tempfile()
  cat("{}", file = fname, fill = TRUE)
  expect_error(read_config(fname, select = "foo"), "Didn't find key")
  expect_error(read_config(fname), "Didn't find key")
})

test_that("read_config doesn't set environment variables if setenv = FALSE", {
  fname <- tempfile()
  cat("{\"LINE1\": \"line1_value\", \"LINE2\": \"line2_value\"}",
    file = fname, fill = TRUE)
  Sys.setenv("LINE1" = "")
  Sys.setenv("LINE2" = "")
  expect_equal(read_config(fname, setenv = FALSE), list(LINE1 = "line1_value",
      LINE2 = "line2_value"))
  expect_equal(Sys.getenv("LINE1"), "")
  expect_equal(Sys.getenv("LINE2"), "")
})

test_that("read_config passes along parsing errors", {
  fname <- tempfile()
  cat("", file = fname, fill = TRUE)
  expect_error(read_config(fname), "Error parsing.*parse error")
})
