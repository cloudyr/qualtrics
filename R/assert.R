#' @import assertthat
assert_that <- assertthat::assert_that
see_if <- assertthat::see_if
is.string <- assertthat::is.string
is.flag <- assertthat::is.flag
has_name <- assertthat::has_name

is_qualtrics_design <- function(x) {
  assert_that(inherits(x, "qualtrics_design"))
}

is_text <- function(x) {
  # Return TRUE if x is a positive-length string
  assert_that(is.string(x))
  nchar(x) > 0
}

assertthat::on_failure(is_text) <- function(call, env) {
  # If is_text returns FALSE, show the name of the variable passed to its
  # argument x.
  #
  # Given `a=""; is_text(a)`, the error message should be "'a' is not a
  # positive-length string", rather than showing the string value ('') or
  # argument name 'x').
  paste0("'", call$x, "' is not a positive-length string")
}

# is_token is a variant of is_text with an error message for use with API tokens
is_token <- is_text
assertthat::on_failure(is_token) <- function(call, env) {
  paste0("The Qualtrics API token should be a positive-length string. ",
         "Set the environment variable QUALTRICS_TOKEN or use the 'token' ",
         "argument of the function making an API request. For more see ",
         "help('read_config').")
}

is_action <- function(x) {
  assert_that(is_text(x))
  for (action in valid_actions) {
    if (nchar(x) >= nchar(action) && substr(x, 1, nchar(action)) == action) {
      return(TRUE)
    }
  }
  return(FALSE)
}

assertthat::on_failure(is_action) <- function(call, env) {
  paste(deparse(call$x), "is not a valid API action. For more see ",
        "help('request')")
}

valid_actions = c("contacts",
                  "directories",
                  "distributions",
                  "divisions",
                  "eventsubscriptions",
                  "groups",
                  "libraries",
                  "mailinglists",
                  "organizations",
                  "responseexports",
                  "responseimports",
                  "responses",
                  "surveys",
                  "users")


