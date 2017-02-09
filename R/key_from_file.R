#' Set the environment variable for the API key from a file
#'
#' The environment variable \code{QUALTRICS_KEY} should give an API key for
#' communication with the Qualtrics survey platform. \code{key_from_file} sets
#' it using the contents of a file whose first line contains the API key.
#'
#' The environment variable for the API key can be set during
#' \link[=Startup]{startup} (recommended), interactively with
#' \code{\link[base]{Sys.setenv}} or through \code{key_from_file}.
#'
#' @param path A file path.
#' @param verbose Whether to print the result of reading from \code{path}, or
#'   not (the default).
#'
#' @return Invisibly returns the value of the environment variable
#'   \code{QUALTRICS_KEY}, after setting it as a side effect.
#' @export
key_from_file <- function(path = path.expand("~/.qualtrics_key"), verbose =
  FALSE) {
  if (file.exists(path)) {
    f_lines <- readLines(path)
    if (length(f_lines) && is_key(f_lines[1])) {
      # the file and its first line have positive length
      Sys.setenv("QUALTRICS_KEY" = f_lines[1])
      if (isTRUE(verbose)) {
        message("Set API key from ", path)
      }
    } else {
      # the first line of the file doesn't contain text
      stop("Couldn't find key in the first line of ", path)
    }
  } else {
    # path doesn't exist
    if (sys.parent() > 1) {
      # key_from_file was called by another function, probably because the
      # QUALTRICS_KEY environment variable hasn't been set; explain the error
      stop("Set the environment variable QUALTRICS_KEY or use the 'key' ",
        "argument of the function making an API request. For more see ",
        "help('key_from_file').")
    } else {
      # key_from_file was called directly
      stop("Couldn't read ", path)
    }
  }
  invisible(Sys.getenv("QUALTRICS_KEY"))
}

