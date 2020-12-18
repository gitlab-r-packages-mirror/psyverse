#' Convert a DCT object to YAML
#'
#' @param dctObject The DCT object
#'
#' @return A character vector.
#'
#' @export
dct_object_to_yaml <- function(dctObject) {

  if (!class(dctObject) == "psyverse_dct") {
    stop("Object dctObject must have class `psyverse_dct`, but has class ",
         vecTxtQ(class(dctObject)),
         ".");
  }

  yaml <-
    yaml::as.yaml(list(dct = dctObject));

  return(yaml);

}
