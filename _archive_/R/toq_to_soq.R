#' Import a TOQ spec and convert it to a SOQ spec
#'
#' Import a Tabulated Open Questionnaire specification (e.g. in a
#' spreadsheet) and convert it into a Serialized Open Questionnaire
#' specification (as an R object or in YAML format).
#'
#' @param x The path to a spreadheet file or the URL to a google sheet that
#' is publicly accessible.
#' @param returnYAML Whether to return an R object (`FALSE`) or YAML (`TRUE`).
#'
#' @return A character vector holding the YAML.
#' @export
#'
#' @examples \donttest{
#' soq <-
#'   psyverse::toq_to_soq(
#'     paste0(
#'       "https://docs.google.com/spreadsheets/d/",
#'       "1temqfgkUqWypzjsvvLKMkjR707An1Vt-jzK96WBUsPQ"
#'     ),
#'     returnYAML = TRUE
#'   );
#' }
#'
toq_to_soq <- function(x,
                       returnYAML = FALSE) {

  toq <- psyverse::read_spreadsheet(x);

  standardSheetNames <- c("metadata",
                          "items",
                          "response_registration_templates",
                          "adapters",
                          "flanking_content",
                          "content_types");

  allSheetNames <- names(toq);

  otherSheetNames <- setdiff(allSheetNames, standardSheetNames);

  missing_UIIDs <-
    is.na(toq$items$uiid) |
    (nchar(toq$items$uiid)< 3);

  toq$items$uiid[missing_UIIDs] <-
    paste0(toq$items$uqid_prefix[missing_UIIDs],
           "_",
           psyverse::generate_ids(sum(missing_UIIDs)));


  if (!all(standardSheetNames %in% allSheetNames)) {

    stop("Not all required worksheets exist in the spreadsheet ",
         "you loaded that should contain the TOQ!");

  }

  responseRegistrationTemplateIds <-
    unique(toq$response_registration_templates$template_id);

  responseRegistrationTemplates <-
    lapply(
      responseRegistrationTemplateIds,
      function(rrTid) {
        tmpDf <-
          toq$response_registration_templates[
            toq$response_registration_templates$template_id == rrTid,
          ];
        tmpDf <- tmpDf[, setdiff(names(tmpDf), "template_id")];
        tmpDf <-
          serialize_df(tmpDf);
        return(tmpDf);
      }
    );
  names(responseRegistrationTemplates) <-
    responseRegistrationTemplateIds;

  metadata <- list();
  metadataFields <- unique(toq$metadata$metadata_field);
  metadata <- lapply(
    metadataFields,
    \(x) toq$metadata$metadata_content[toq$metadata$metadata_field == x]
  );
  names(metadata) <- metadataFields;

  res <-
    list(metadata = metadata, #stats::setNames(as.list(toq$metadata$metadata_content),
                               #     nm = toq$metadata$metadata_field),
         items = serialize_df(toq$items),
         response_registration_templates = responseRegistrationTemplates,
         adapters = serialize_df(toq$adapters),
         flanking_content = serialize_df(toq$flanking_content),
         content_types = serialize_df(toq$content_types));

  if (length(otherSheetNames) > 0) {
    res <- c(
      res,
      lapply(
        toq[otherSheetNames],
        serialize_df
      )
    );
  }

  if (is.null(res$metadata$uqid) || is.na(res$metadata$uqid) ||
      (nchar(res$metadata$uqid) < 3)) {
    res$metadata$uqid <-
      psyverse::generate_id(res$metadata$uqid_prefix);
  }

  if (returnYAML) {
    return(
      yaml::as.yaml(
        res
      )
    );
  } else {
    return(res);
  }

}
