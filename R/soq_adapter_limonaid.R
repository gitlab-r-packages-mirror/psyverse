#' SOQ adapter for LimeSurvey (using the {limonaid} package)
#'
#' @param soq The SOQ specification
#'
#' @return The .LSG content as {xml2} XML object.
#' @export
#'
#' @examples \donttest{
#' UQID <- "eq60eng_7rs8g3bd";
#'
#' all_soq_specs <-
#'   psyverse::read_SOQs_from_url();
#'
#' selected_soq <- all_soq_specs[[UQID]];
#'
#' lsgContent <-
#'   psyverse::soq_adapter_limonaid(
#'     selected_soq
#'   );
#' }
#'
soq_adapter_limonaid <- function(soq) {

  if (!requireNamespace("limonaid", quietly = TRUE)) {
    stop("You need the {limonaid} package to be able to import from ",
         "a questionnaire repo, and at least version 0.2.");
  }

  adapterInfo <- soq$adapters[soq$adapters$target_format == "limonaid", ];

  if ("questionType" %in% adapterInfo$field) {
    questionTypeFields <- adapterInfo[adapterInfo$field == "questionType", c("uiid", "content")];
    questionTypeFields <- stats::setNames(questionTypeFields$content,
                                          nm = questionTypeFields$uiid);
    if ("*" %in% names(questionTypeFields)) {
      lsType_per_item <- rep(questionTypeFields["*"], nrow(soq$items));
    } else {
      lsType_per_item <- rep(NA, nrow(soq$items));
    }

  } else {
    warning("No question types specified in the adapter information! Assuming radio buttons.");
    lsType_per_item <- rep("radio", nrow(soq$items));
  }
  names(lsType_per_item) <- soq$items$uiid;

  lsGroup <- limonaid::Group$new();

  browser();

}
