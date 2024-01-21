
read_json_from_url <- function(url,
                               elementId = NULL,
                               elementClass = NULL,
                               attribute) {

  html <- xml2::read_html(x = url);

  if (is.null(elementId) && is.null(elementClass)) {
    stop("Provide either one of `elementId` or `elementClass`!");
  } else if (!is.null(elementId)) {
    XPath <- paste0('//*[@id="', elementId, '"]');
  } else if (!is.null(elementClass)) {
    XPath <- paste0('//*[contains(@class,"', elementClass, '")]');
  }

  selectedElements <- xml2::xml_find_all(x = html, xpath = XPath);

  #elementId <- paste0("operationalizations-com-data-", uqid);
  #cssId <- paste0('#', elementId);
  #operationalizationElement <- rvest::html_elements(html, css=cssId);


  #XPathClass <- paste0('//*[@id="', elementId, '"]');
  #operationalizationElement <- rvest::html_elements(html, xpath=XPathId);

  # operationalizationElement <- rvest::html_elements(html, css = ".operationalizations-com-json");

  if (length(selectedElements) == 0) {
    stop("No embedded JSON code for a questionnaire with UQID '",
         uqid, "' present at the url (", url, ").")
  } else {
    res <-
      lapply(
        selectedElements,
        function(x) {
          return(
            jsonlite::fromJSON(
              xml2::xml_attr(
                x,
                attribute
              )
            )
          );
        }
      );
  }

  if (length(selectedElements) == 1) {
    res <- res[[1]];
  }

  return(res);

}


test1 <-
read_json_from_url("https://operationalizations.com/questionnaires/json",
                   elementId="operationalizations-com-data-bfi44eng_7rrlcsyp",
                   attribute = "data-operationalizations-com")

test2 <-

  read_json_from_url("https://operationalizations.com/questionnaires/json",
                   elementClass = "operationalizations-com-json",
                   attribute = "data-operationalizations-com")

identical(test1, test2[[1]]);

