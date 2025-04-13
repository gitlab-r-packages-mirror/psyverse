#' Create an HTML fragment showing a DCT object
#'
#' @param dctObject The DCT object
#' @inheritParams generate_construct_overview
#'
#' @return A character vector.
#'
#' @export
#'
#' @examples exampleDCT <-
#'   psyverse::dct_object(
#'     prefix = "exampleConstruct",
#'     label = "An example construct",
#'     definition = "The definition goes here",
#'     measure_dev = "Here you can explain how to measure the construct"
#'   );
#' ### Only run this in an interactive R session,
#' ### as it shows the HTML in the viewer.
#' if (interactive()) {
#'   dct_object_to_html(exampleDCT);
#' }
dct_object_to_html <- function(dctObject,
                               headingLevel = 3,
                               hyperlink_UCIDs = TRUE,
                               collapseButtons = TRUE,
                               urlPrefix = "#",
                               sortDecreasing = FALSE) {

  if (!inherits(dctObject, "psyverse_dct")) {
    stop("Object dctObject must have class `psyverse_dct`, but has class ",
         vecTxtQ(class(dctObject)),
         ".");
  }

  res <-
    generate_construct_overview(
      dctObject,
      headingLevel=headingLevel,
      hyperlink_UCIDs = hyperlink_UCIDs,
      HTMLoutput = TRUE,
      collapseButtons = collapseButtons,
      urlPrefix = urlPrefix,
      sortDecreasing = sortDecreasing
    );

  class(res) <- c("psyverse_html", class(res));

  return(res);

}
