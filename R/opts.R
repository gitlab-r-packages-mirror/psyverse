#' Options for the psyverse package
#'
#' The `psyverse::opts` object contains three functions to set, get, and reset
#' options used by the escalc package. Use `psyverse::opts$set` to set options,
#' `psyverse::opts$get` to get options, or `psyverse::opts$reset` to reset specific or
#' all options to their default values.
#'
#' It is normally not necessary to get or set `psyverse` options.
#'
#' The following arguments can be passed:
#'
#' \describe{
#'   \item{...}{For `psyverse::opts$set`, the dots can be used to specify the options
#'   to set, in the format `option = value`, for example,
#'   `encoding = "UTF-8"`. For
#'   `psyverse::opts$reset`, a list of options to be reset can be passed.}
#'   \item{option}{For `psyverse::opts$set`, the name of the option to set.}
#'   \item{default}{For `psyverse::opts$get`, the default value to return if the
#'   option has not been manually specified.}
#' }
#'
#' The following options can be set:
#'
#' \describe{
#'
#'   \item{encoding}{The default encoding used to read or write files.}
#'
#' }
#'
#' @aliases opts set get reset
#'
#' @usage opts
#'
#' @examples ### Get the default encoding
#' psyverse::opts$get(encoding);
#'
#' ### Set it to UTF-8-BOM
#' psyverse::opts$set(encoding = "UTF-8-BOM");
#'
#' ### Check that it worked
#' psyverse::opts$get(encoding);
#'
#' ### Reset this option to its default value
#' psyverse::opts$reset(encoding);
#'
#' ### Check that the reset worked, too
#' psyverse::opts$get(encoding);
#'
#' @export
opts <- list();

opts$set <- function(...) {
  dots <- list(...);
  dotNames <- names(dots);
  names(dots) <-
    paste0("psyverse.", dotNames);
  if (all(dotNames %in% names(opts$defaults))) {
    do.call(options,
            dots);
  } else {
    stop("Option '", option, "' is not a valid (i.e. existing) option for psyverse!");
  }
}

opts$get <- function(option, default=FALSE) {
  option <- as.character(substitute(option));
  if (!option %in% names(opts$defaults)) {
    stop("Option '", option, "' is not a valid (i.e. existing) option for psyverse!");
  } else {
    return(getOption(paste0("psyverse.", option),
                     opts$defaults[[option]]));
  }
}

opts$reset <- function(...) {
  optionNames <-
    unlist(lapply(as.list(substitute(...())),
                  as.character));
  if (length(optionNames) == 0) {
    do.call(opts$set,
            opts$defaults);
  } else {
    prefixedOptionNames <-
      paste0("psyverse.", optionNames);
    if (all(optionNames %in% names(opts$defaults))) {
      do.call(opts$set,
              opts$defaults[optionNames]);
    } else {
      invalidOptions <-
        !(optionNames %in% names(opts$defaults));
      stop("Option(s) ", vecTxtQ(optionNames[invalidOptions]),
           "' is/are not a valid (i.e. existing) option for psyverse!");
    }
  }
}

opts$defaults <-
  list(
    ### Encoding for files
    encoding = "UTF-8",
    preventOverwriting = TRUE,

    ### Whether to be silent or chatty
    silent = TRUE,

    ### Column names for DCT spreadsheets
    dct_sheet_fieldCol = "field",
    dct_sheet_contentCol = "content",

    ### Regular expressions for Google Sheets
    gSheetId_extractionRegex =
      "^https://docs\\.google\\.com/spreadsheets/d/([a-zA-Z0-9_-]*)(/.*)?$",

    gSheetId_to_exportLink =
      "https://docs.google.com/spreadsheets/d/%s/export?format=xlsx",

    ### Throttling for google spreasheets
    throttleSeconds = 10,

    ### color to use for the background when exporting to html
    exportHTMLbackground = "white",



    ### Cognitive Interview: Narrative Response Models
    nrm_wsNames = list(
      metadata = "metadata",
      instrument = "instrument",
      probes = "probes",
      stimuli = "stimuli",
      operationalizations = "operationalizations",
      responsemodel_prototype = "responsemodel_prototype",
      responsemodels= "responsemodels",
      response_options = "response_options"
    ),

    nrm_colNames = list(
      metadata = c(
        metadata_field = 'metadata_field',
        metadata_content = 'metadata_content'
      ),
      instrument = c(
        item_sequence = 'item_sequence',
        item_id = 'item_id',
        item_template_nrm = 'item_template_nrm'
      ),
      probes = c(
        item_id = 'item_id',
        responsemodel_id = 'responsemodel_id',
        stimulus_id = 'stimulus_id',
        probe_id = 'probe_id',
        probe_target = 'probe_target',
        probe_ci_category = 'probe_ci_category',
        probe_ambiguity = 'probe_ambiguity',
        probe_label = 'probe_label'
      ),
      stimuli = c(
        item_id = 'item_id',
        stimulus_id = 'stimulus_id',
        stimulus_content = 'stimulus_content',
        stimulus_language = 'stimulus_language',
        stimulus_function = 'stimulus_function',
        stimulus_alias = 'stimulus_alias'
      ),
      operationalizations = c(
        item_id = 'item_id',
        operationalization_label = 'operationalization_label',
        operationalization_description = 'operationalization_construct',
        operationalization_comments = 'operationalization_comments'
      ),
      responsemodel_prototype = c(
        responsemodel_id = 'responsemodel_id',
        responsemodel_sequence = "responsemodel_sequence",
        responsemodel_label = 'responsemodel_label',
        responsemodel_comments = 'responsemodel_comments'
      ),
      responsemodels = c(
        item_id = 'item_id',
        responsemodel_sequence = "responsemodel_sequence",
        responsemodel_id = 'responsemodel_id',
        responsemodel_label = 'responsemodel_label',
        responsemodel_comments = 'responsemodel_comments'
      ),
      response_options = c(
        item_id = "item_id",
        response_option_template = "response_option_template",
        response_option_sequence = "response_option_sequence",
        response_option_value = "response_option_value"
      )
    ),

    ### For CI template replacements
    ci_template_replacementDelimiters = c("<<", ">>"),
    rpe_mq_idName = "prbid",
    nrm_probe_idName = "prbid",

    uiid_idName = "uiid",
    rpe_iterId = "iterId",
    rpe_batchId = "batchId",
    rpe_popId = "popId",
    rpe_mq_idName = "prbid",
    coderId_name = "coderId",
    caseId_name = "caseId",

    rpe_itemEval_template = "### Coder evaluation

[[eval|| ]]

[[comment||none]]
"


  )

