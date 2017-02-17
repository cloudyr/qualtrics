#' Set API environment variables from a configuration file
#'
#' The environment variable \code{QUALTRICS_TOKEN} should give an
#' \href{https://api.qualtrics.com/docs/authentication}{API token} for
#' authentication. \code{QUALTRICS_SUBDOMAIN} should give the subdomain(s) that
#' precede \code{qualtrics.com} in your
#' \href{https://api.qualtrics.com/docs/root-url}{API URL}.
#' \code{read_config} sets these environment variables using the contents of a
#' JSON-formatted configuration file.
#'
#' An example configuration file:
#'
#' \code{
#' \{
#'   "QUALTRICS_TOKEN": "secret_token",
#'   "QUALTRICS_SUBDOMAIN": "co1"
#' \}
#' }
#'
#' Environment variables can be set during \link[=Startup]{startup}
#' (recommended), interactively with \code{\link[base]{Sys.setenv}} or through
#' \code{read_config}.
#'
#' @param path A file path.
#' @param select Keys to select from the configuration file, optionally. 
#' @param setenv Whether to set the environment variables using the
#' configuration file contents as a side effect (the default), or not.
#' @param verbose Whether to output the names of the environment variables read
#' from the configuration file, or not (the default).
#'
#' @return Invisibly returns a named list of values read from the configuration
#' file, and sets the name-value pairs as environment variables, if argument
#' \code{setenv} is \code{TRUE} (the default).
#' @importFrom jsonlite read_json
#' @export
read_config <- function(path = path.expand("~/.qualtrics_api"), select = NULL,
  setenv = TRUE, verbose = FALSE) {
  assert_that(is.string(path))
  if (length(select)) assert_that(is.character(select))
  if (file.exists(path)) {
    config <- tryCatch(jsonlite::read_json(path), error = function(e) 
      stop("Error parsing configuration file. ", e))
    if (length(select)) {
      # subset config to keys named in select
      config <- config[names(config) %in% select]
    }
    if (!length(config)) {
      stop("Didn't find key(s) in configuration file.")
    }
    if (isTRUE(setenv)) {
      do.call(Sys.setenv, config)
      if (isTRUE(verbose)) {
        message("Set from file: ", paste(names(config), collapse = ", "))
      }
    }
  } else {
    # path doesn't exist
    if (sys.parent() > 1) {
      # read_config was called by another function, probably because the
      # QUALTRICS_TOKEN environment variable hasn't been set; explain the error
      stop("Set the environment variables QUALTRICS_TOKEN and ",
        "QUALTRICS_SUBDOMAIN or use the 'token' and 'subdomain' arguments of ",
        "the function making an API request. For more see ",
        "help('read_config').")
    } else {
      # read_config was called directly
      stop("Couldn't read ", path)
    }
  }
  invisible(config)
}

