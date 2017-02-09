test_that("request minimally works as expected", {
  expect_silent(action_response <- request("GET", action = "surveys"))
  expect_match(action_response$url, "/surveys$")
  expect_equal(action_response$request$method, "GET")
})

test_that("action must give either a known action or a valid URL", {
  expect_error(request("GET", action = "foo"),
    ".*not a valid Qualtrics API URL. \\(Can't parse hostname.\\).*")
  expect_error(request("GET", action = "https://az1.qualtrics.com/"),
    ".*not a valid Qualtrics API URL. \\(Can't parse path.\\).*")
  expect_error(request("GET", action = "/some_path/"),
    ".*not a valid Qualtrics API URL. \\(Can't parse hostname.\\).*")
})

test_that("verb must be an expected verb", {
  expect_error(request("surprise"), "should be one of")
})

test_that("... argument is passed to httr::VERB", {
  response <- request("GET", "surveys", query = "q=value")
  expect_is(response, "response")
  expect_match(response$url, "\\?q=value$")
})

test_that("if argument 'action' is an empty string, don't make a request", {
  expect_error(request("GET", ""), "'action' is not a positive-length string")
})

test_that("request verbosity works", {
  expect_message(request("GET", "surveys", verbose = TRUE),
    "Sending GET request")
})

test_that("stop_for_status stops for bad status", {
  response <- list(status_code = 404, content = charToRaw("foo"),
    headers = list("Content-Type" = "raw", "Encoding" = "UTF-8"))
  class(response) <- "response"
  expect_error(stop_for_status(response), "Not Found")
})

test_that("warn_on_notice shows response$meta$notice if it exists", {
  response <- readRDS('response.Rds')
  expect_silent(warn_on_notice(response))

  response_with_notice <- readRDS('response_with_notice.Rds')
  expect_warning(warn_on_notice(response_with_notice), "^Request proxied")
})

test_that("add_qheaders works as expected", {
  response <- add_qheaders("key value")
  expect_true("content_type" %in% names(response$headers))
  expect_true("x-api-token" %in% names(response$headers))
  expect_equal(unname(response$headers["content_type"]), "application/json")
  expect_equal(unname(response$headers["x-api-token"]), "key value")
})

test_that("default_if_missing works as expected", {
  expect_silent(default_if_missing("value"))
  expect_equal(suppressWarnings(default_if_missing("")), "az1")
  expect_warning(default_if_missing(""), "Set the environment variable")
})

test_that("build_api_url works as expected", {
  expect_equal(build_api_url("surveys", "az1"),
    "https://az1.qualtrics.com/API/v3/surveys")
  expect_error(build_api_url("surveys", ""),
    "'subdomain' is not a positive-length string")
  expect_error(build_api_url("", "az1"),
    "'action' is not a positive-length string")
})

test_that("parse_action works as expected", {
  expect_equal(parse_action("surveys", "co1"),
    "https://co1.qualtrics.com/API/v3/surveys")
  expect_equal(parse_action("https://co1.qualtrics.com/API/v3/surveys", "foo"),
    "https://co1.qualtrics.com/API/v3/surveys")
})

test_that("is_action works as expected", {
  expect_true(is_action("users"))
  expect_false(is_action("/users"))
  expect_true(is_action("users/4510"))
  expect_false(is_action("eventsub"))
})

test_that("check_api_url works as expected", {
  # incomplete hostname is an error
  expect_error(check_api_url("co1.qualtrics.com/API/v3/"),
    "Can't parse hostname.")
  # missing path is an error
  expect_error(check_api_url("https://co1.qualtrics.com/"),
    "Can't parse path.")
})
