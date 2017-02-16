test_that("read_config reads from a file as expected", {
  original_token <- Sys.getenv("QUALTRICS_TOKEN")
  fname <- tempfile()
  cat("{\"QUALTRICS_TOKEN\": \"token_value\"}", file = fname, fill = TRUE)

  expect_silent(value <- read_config(fname))
  expect_equal(value, "QUALTRICS_TOKEN")
  expect_message(read_config(fname, verbose = TRUE), "Set from file")

  cat("{\"LINE1\": \"line1_value\", \"LINE2\": \"line2_value\"}",
    file = fname, fill = TRUE)
  expect_equal(read_config(fname), c("LINE1", "LINE2"))
  expect_equal(Sys.getenv("LINE1"), "line1_value")
  expect_equal(Sys.getenv("LINE2"), "line2_value")

  Sys.setenv("QUALTRICS_TOKEN" = original_token)
})

test_that("read_config passes along parsing errors", {
  fname <- tempfile()
  cat("", file = fname, fill = TRUE)
  expect_error(read_config(fname), "Error parsing.*parse error")
})
