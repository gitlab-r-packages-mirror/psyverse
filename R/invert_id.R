#' Invert identifier
#'
#' Invert the identifier for one or more constructs.
#'
#' @param x The identifier(s) as a character vector.
#'
#' @return The identifier(s) as a numeric vector.
#' @export
#'
#' @examples
#' invert_id(generate_id('example'));
invert_id <- function(x) {
  datetimeBit <- gsub("^.*_([^_]+)$",
                      "\\1",
                      x);
  return(base30toNumeric(datetimeBit));
}
