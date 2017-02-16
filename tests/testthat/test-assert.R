test_that("is_qualtrics_design works as expected", {
  x <- list()
  expect_error(is_qualtrics_design(x),
    "x does not inherit from class qualtrics_design")
  class(x) <- "qualtrics_design"
  expect_silent(is_qualtrics_design(x))
})

test_that("asserting is_text works as expected", {
  a = "foo"
  expect_true(assert_that(is_text(a)))
  b = ""
  expect_error(assert_that(is_text(b)), "'b' is not a positive-length string")
  d = 0
  expect_error(assert_that(is_text(d)), "is not a string")
})

test_that("is_token works as expected", {
  expect_true(assert_that(is_token("value")))
  expect_error(assert_that(is_token("")), "^The Qualtrics API token")
})

