#' Save a psyverse object or YAML character vector to a file
#'
#' Pretty much what it says on the box. But check the but about encoding.
#'
#' @param x The object to save.
#' @param file The file to save to.
#' @param preventOverwriting Whether to prevent overwriting.
#' @param encoding The encoding to use. Note that in general, encoding seems
#' to have been invented primarily as a source of frustration, and it rarely
#' disappoints. If unsure, use UTF-8. If using UTF-8, the approach from
#' \url{https://kevinushey.github.io/blog/2018/02/21/string-encoding-and-r/} will
#' be used.
#'
#' @return The character vector that was written to the file.
#' @export
#'
#' @examples
save_to_yaml <-
  function(
    x,
    file,
    preventOverwriting = psyverse::opts$get("preventOverwriting"),
    encoding = psyverse::opts$get("encoding")
  ) {

  if (class(x) == "psyverse_dct") {
    x <- dct_object_to_yaml(x);
  }

  x <-
    c("---",
      x,
      "---",
      "");

  if ((!file.exists(file)) || (!preventOverwriting)) {

    if (encoding == "UTF-8") {
      x <- enc2utf8(x);
      con <- file(file, open = "w", encoding="native.enc");
      writeLines(x, con = con, useBytes=TRUE);
      close(con);
    } else {
      con <- file(file, open = "w", encoding=encoding);
      writeLines(x, con = con, useBytes=FALSE);
      close(con);
    }

  } else {
    cat("File '", file,
        "' exists, and preventOverwriting ",
        "is not set to TRUE, so did not save file.");
  }

  return(invisible(x));

}
