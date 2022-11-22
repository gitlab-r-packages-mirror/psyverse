#' Display HTML
#'
#' This function displays HTML in the viewer, adding `<body>` and `<head>`
#' tags (which should therefore not be included in the fragment).
#'
#' @param x The HTML fragment
#' @param title The title
#' @param css CSS
#'
#' @return Invisibly, `x`, with the extra HTML bits added.
#' @export
#'
#' @examples psyverse::viewHTML("<strong>Hello world!</strong>");
viewHTML <- function(x,
                     title = "Psyverse",
                     css = "body {font-size: 16px;}") {

  x <- paste0("<!doctype html>
<html lang=en>
<head>
<meta charset=utf-8>
<title>", title, "</title>
<style>", css, "</style>
</head>
<body>
", x, "
</body>
</html>");

  tmpFile <- tempfile(fileext = ".html");

  writeLines(x, tmpFile);

  if ((requireNamespace("rstudioapi", quietly = TRUE)) &&
      (rstudioapi::isAvailable())) {
    viewer <- rstudioapi::viewer;
  } else {
    viewer <- getOption("viewer",
                        utils::browseURL);
  }

  viewer(tmpFile);

  return(invisible(x));
}

#' @method print psyverse_html
#' @export
print.psyverse_html <- function(x,
                                ...) {
  psyverse::viewHTML(x);
  return(invisible(x));
}
