#' Send a request to the Qualtrics API
#'
#' `request` is a lower-level function for direct access to API actions. It may
#' be useful in programming.
#'
#' For help with subdomains, see the
#' \href{https://api.qualtrics.com/docs/root-url}{Qualtrics documentation}. Each
#' Qualtrics account is assigned a subdomain, and using another will work but
#' produce a warning.
#'
#' @inheritParams httr::VERB
#' @param action Either an API action (like \code{"surveys"}) or a complete API
#'   URL.
#' @param key A Qualtrics API key (by default, the value of the environment
#'   variable \code{QUALTRICS_KEY}).
#' @param subdomain A Qualtrics subdomain (by default, the value of the
#'   environment variable \code{QUALTRICS_SUBDOMAIN}). Ignored if `action` is a
#'   complete API URL.
#' @param verbose Output API calls to \code{\link[base]{stderr}}.
#' @param ... Further arguments to the function given by \code{verb}, e.g.,
#'   \code{\link[httr]{GET}}.
#'
#' @return For \code{request}, a \code{\link[httr]{response}} object. For
#'   \code{qget} and \code{qpost}, its content as extracted by
#'   \code{\link[httr]{content}}.
#' @importFrom httr GET POST VERB add_headers content modify_url stop_for_status
#' @export
#' @examples
#' \dontrun{
#' response <- request("GET", "surveys")
#' }
request <- function(verb = c("GET", "POST", "PUT", "DELETE"),
  action,
  key = Sys.getenv("QUALTRICS_KEY"),
  subdomain = Sys.getenv("QUALTRICS_SUBDOMAIN"),
  verbose = FALSE,
  ...) {

  read_if_missing(key)
  subdomain <- default_if_missing(subdomain)

  verb <- match.arg(verb)
  assert_that(is_text(verb))
  assert_that(is_text(action))
  assert_that(is_text(subdomain))
  assert_that(is.flag(verbose))

  api_url <- parse_action(action, subdomain)

  if (verbose) {
    message("Sending ", verb, " request to ", api_url)
  }

  response <- httr::VERB(verb,
    api_url,
    add_qheaders(key),
    encode = ifelse(identical(verb, "POST"), "json", NULL),
    ...)

  stop_for_status(response)
  warn_on_notice(response)
  return(response)
}

read_if_missing <- function(key) {
  # If argument key doesn't give a positvie-length string, try to read an API
  # key from file using key_from_file().
  #
  # This function is called from an exported function that has an argument 'key'
  # defaulting to the environment variable QUALTRICS_KEY.
  assertthat::assert_that(assertthat::is.string(key))
  if (key == "") {
    key_from_file()
  } else {
    return(TRUE)
  }
}

stop_for_status <- function(response) {
  # Provide user-friendly stop() messages for some error codes. Catch all other
  # errors with httr::stop_for_status().
  # 
  # @param response A httr response object.
  status_message <- paste0("The Qualtrics API responded \"",
    httr::http_status(response)$message, "\". ")
  please_report <- c("If you aren't programming your own requests with ",
    "request(), please report this error at ",
    "<https://github.com/cloudyr/qualtrics/issues>.")
  assert_that(has_name(response, "status_code"))
  code <- response$status_code
  assert_that(is.numeric(code))
  if (code %in% c(400, 413)) {
    # Bad Request, Payload Too Large
    stop(status_message, please_report)
  } else if (code == 404) {
    # Not Found
    stop("Requested <", response$request$url, ">. ", status_message, 
      please_report)
  } else if (code == 401) {
    # Unauthorized
    stop(status_message, "Please check your API key.")
  } else if (code == 429) {
    # Too Many Requests
    stop(status_message, "You have reached the request rate limit.")
  } else if (code >= 500) {
    # Server Error
    stop(status_message, "You may want to contact Qualtrics Support at ",
      "<https://www.qualtrics.com/contact>.")
  }
  httr::stop_for_status(response)
}

warn_on_notice <- function(response) {
  # If the meta element of the request response content has a 'notice' element,
  # pass it on to the user as a warning. Known to happen if an incorrect
  # subdomain is used.
  assert_that(inherits(response, "response"))
  content <- httr::content(response)
  if ("meta" %in% names(content) && "notice" %in% names(content$meta)) {
    warning(content$meta$notice)
  }
}

add_qheaders <- function(key) {
  # Add Qualtrics headers to a httr request
  assert_that(is_key(key))
  httr::add_headers("content_type" = "application/json", "x-api-token" = key)
}

default_if_missing <- function(subdomain) {
  # If the subdomain is a length-zero string, the environment variable
  # QUALTRICS_SUBDOMAIN hasn't been (properly) set. Return a valid subdomain to
  # be used (\code{az1}) and throw a warning.
  assert_that(is.string(subdomain))
  if (subdomain == "") {
    warning("Set the environment variable QUALTRICS_SUBDOMAIN for better performance. ",
      "See https://api.qualtrics.com/docs/root-url.")
    subdomain = "az1"
  }
  return(subdomain)
}

parse_action <- function(action, subdomain) {
  # Determine whether 'action' gives the name of an action (e.g. "surveys") or a
  # complete API URL. 
  assert_that(is_text(action))
  if (is_action(action)) {
    # action gives a known action; construct the rest of the URL
    api_url <- build_api_url(action, subdomain)
  } else {
    # action should give a complete URL; check that it has a hostname and path
    check_api_url(action)
    api_url <- action
  }
  return(api_url)
}

build_api_url <- function(action, subdomain, query = NULL) {
  # Build a Qualtrics API URL from specified and fixed parts
  assert_that(is_text(action))
  assert_that(is_text(subdomain))
  httr::modify_url("",
    scheme = "https",
    hostname = paste(subdomain, "qualtrics.com", sep = "."),
    path = paste("API/v3", action, sep = "/"),
    query = query)
}

check_api_url <- function(url_string) {
  assert_that(is_text(url_string))
  parsed <- httr::parse_url(url_string)
  error_stem <- paste0("\"", url_string, "\" ",
    "is not a valid Qualtrics API URL.")
  error_stub <- "For more see help('request')."
  if (!length(parsed$hostname) || !is_text(parsed$hostname)) {
    stop(error_stem, " (Can't parse hostname.) ", error_stub)
  } else if (!length(parsed$path) || !is_text(parsed$path)) {
    stop(error_stem, " (Can't parse path.) ", error_stub)
  }
}

