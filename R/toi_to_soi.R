#' Import a TOI spec and convert it to a SOI spec
#'
#' Import a Tabulated Open Item specification (e.g. in a
#' spreadsheet) and convert it into a Serialized Open Item
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
#' soi <-
#'   psyverse::toi_to_soi(
#'     paste0(
#'       "https://docs.google.com/spreadsheets/d/",
#'       "1T7UN7tiLV89YXMglXdC7Cu3nCAijt_1mV93ehwM3ybE"
#'     ),
#'     returnYAML = TRUE
#'   );
#' }
#'
toi_to_soi <- function(x,
                       returnYAML = FALSE) {

  toi <- psyverse::read_spreadsheet(x);

  standardSheetNames <- c("metadata",
                          "content",
                          "adapters",
                          "content_types");

  allSheetNames <- names(toi);

  otherSheetNames <- setdiff(allSheetNames, standardSheetNames);

  uiid_prefix <- toi$metadata[toi$metadata$metadata_field == "uiid_prefix", "metadata_content"];
  uiid <- toi$metadata[toi$metadata$metadata_field == "uiid", "metadata_content"];

  if (is.na(uiid) || (nchar(uiid) == 0)) {

    uiid <- psyverse::generate_id(paste0(uiid_prefix, "_"));
    toi$metadata[toi$metadata$metadata_field == "uiid", "metadata_content"] <-
      uiid;

  }


  if (!all(standardSheetNames %in% allSheetNames)) {

    stop("Not all required worksheets exist in the spreadsheet ",
         "you loaded that should contain the TOI!");

  }

  questionText <-
    paste0(toi$content$content[toi$content$content_role == "question_text"],
           collapse = "\n");

  responseOption_df <-
    toi$content[toi$content$content_role == "response_option", ];

  contentColNames <- names(toi$content);

  responseOptions <-
    apply(
      responseOption_df,
      1,
      function(responseOption) {
        res <- as.list(responseOption);
        names(res) <- contentColNames;
        return(res);
      }
    );

  names(responseOptions) <- responseOption_df$identifier;
  responseOptions <-
    responseOptions[order(as.numeric(responseOption_df$response_option_sequence))];

  item <-
    list(id = uiid,
         questionText = questionText,
         responseOptions = responseOptions);

  res <-
    list(metadata = stats::setNames(as.list(toi$metadata$metadata_content),
                                    nm = toi$metadata$metadata_field),
         item = item,
         adapters = serialize_df(toi$adapters),
         content_types = serialize_df(toi$content_types));

  if (length(otherSheetNames) > 0) {
    res <- c(
      res,
      lapply(
        toi[otherSheetNames],
        serialize_df
      )
    );
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
