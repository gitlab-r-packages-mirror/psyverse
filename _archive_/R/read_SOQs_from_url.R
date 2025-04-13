#' Read all SOQs from a URL
#'
#' This function imports all Serialized Open Questionnaire
#' specifications (i.e. SOQ specs) from an online repository.
#'
#' @param url The URL to the repo
#'
#' @return An object of imported SOQs
#' @export
#'
#' @examples \donttest{
#' all_soq_specs <-
#'   psyverse::read_SOQs_from_url();
#' }
read_SOQs_from_url <- function(url = "https://operationalizations.com/questionnaires/json",
                               elementClass = "operationalizations-com-json",
                               attribute = "data-operationalizations-com") {
  SOQs <-
    read_json_from_url("https://operationalizations.com/questionnaires/json",
                       elementClass = elementClass,
                       attribute = attribute);

  if (!is.null(SOQs$metadata)) {
    ### Only one questionnaire imported / available
    SOQs <- list(SOQs);
  }

  UQIDs <- unlist(lapply(SOQs, \(x) x$metadata$uqid));

  names(SOQs) <- UQIDs;

  return(SOQs);

}
