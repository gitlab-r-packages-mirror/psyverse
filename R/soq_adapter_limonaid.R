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
#' tmpFile <- tempfile(
#'   pattern = paste0(UQID, "_"),
#'   fileext = ".lsg"
#' );
#'
#' all_soq_specs <-
#'   psyverse::read_SOQs_from_url();
#'
#' selected_soq <- all_soq_specs[[UQID]];
#'
#' lsgContent <-
#'   psyverse::soq_adapter_limonaid(
#'     selected_soq,
#'     file = tmpFile
#'   );
#'
#' ### You can now import this file as a LimeSurvey group:
#' cat(tmpFile);
#'
#' }
#'
soq_adapter_limonaid <- function(soq,
                                 file = NULL,
                                 silent = psyverse::opts$get("silent")) {

  if (!requireNamespace("limonaid", quietly = TRUE)) {
    stop("You need the {limonaid} package to be able to import from ",
         "a questionnaire repo, and at least version 0.2. You can ",
         "install it with:\n\n  install.packages('limonaid');");
  }

  if (!requireNamespace("ISOcodes", quietly = TRUE)) {
    stop("You need the {ISOcodes} package to be able to export to ",
         "LimeSurvey, since the language needs to be specified. You can ",
         "install it with:\n\n  install.packages('ISOcodes');");
  }

  msg("Questionnaire label: ",
      soq$metadata$label,
      "\n", silent = silent);

  ### More convenient language data frame
  isoLang <- ISOcodes::ISO_639_3;
  row.names(isoLang) <- isoLang$Id;

  if (soq$metadata$language_ISO639_3 %in% ISOcodes::ISO_639_3$Id) {
    msg("Language: ",
        isoLang[soq$metadata$language_ISO639_3, 'eng'],
        " (this has to be the primary language of the survey in which you import this group)",
        "\n", silent = silent);
    langAlpha2 <- isoLang[soq$metadata$language_ISO639_3, 'Part1'];
  } else {
    stop("The specified ISO639-3 language code ('",
         soq$metadata$language_ISO639_3,
         "') is not valid!");
  }

  ### Preparing item data frame

  itemDf <- soq$items;

  itemDf$sequence <-
    tryCatch(
      as.numeric(itemDf$sequence),
      error = \(x) stop("Could not convert the `sequence` values to numeric values!")
    );

  itemDf <- itemDf[order(itemDf$sequence), ];
  row.names(itemDf) <- itemDf$uiid;

  ### Reading adapter information

  adapterInfo <- soq$adapters[soq$adapters$target_format == "limonaid", ];

  ### Question types

  if ("questionType" %in% adapterInfo$field) {
    questionTypeFields <- adapterInfo[adapterInfo$field == "questionType", c("uiid", "content")];
    questionTypeFields <- stats::setNames(questionTypeFields$content,
                                          nm = questionTypeFields$uiid);
    if ("*" %in% names(questionTypeFields)) {
      itemDf$questionType <- rep(questionTypeFields["*"], nrow(soq$items));
    } else {
      itemDf$questionType <- rep(NA, nrow(soq$items));
    }

  } else {
    warning("No question types specified in the adapter information! Assuming radio buttons.");
    itemDf$questionType <- rep("radio", nrow(soq$items));
  }

  ### Regexes for converting UUIDs to LimeSurvey question codes

  if (all(c("uiid_to_code_regex_match", "uiid_to_code_regex_replace") %in% adapterInfo$field)) {

    codeRegex1 <- adapterInfo[adapterInfo$field == "uiid_to_code_regex_match", c("uiid", "content")];
    codeRegex1 <- stats::setNames(codeRegex1$content, nm = codeRegex1$uiid);

    codeRegex2 <- adapterInfo[adapterInfo$field == "uiid_to_code_regex_replace", c("uiid", "content")];
    codeRegex2 <- stats::setNames(codeRegex2$content, nm = codeRegex2$uiid);

    itemDf$codeRegex1 <- rep("(.{1,20})$", nrow(soq$items));
    itemDf$codeRegex2 <- rep("\\1", nrow(soq$items));

    if ("*" %in% names(codeRegex1)) {
      itemDf$codeRegex1 <- rep(codeRegex1["*"], nrow(soq$items));
    }

    if ("*" %in% names(codeRegex2)) {
      itemDf$codeRegex2 <- rep(codeRegex2["*"], nrow(soq$items));
    }

    itemDf$questionCode <-
      apply(itemDf, 1, \(x) {
        sub(x['codeRegex1'], x['codeRegex2'], x['uiid']);
      });

  } else {

    warning("No regular expressions to convert UUIDs into LimeSurvey question codes specified in the adapter information! ",
            "Stripping all non-alphabetic and non-digit characters, taking the last 19 characters, and if ",
            "the first character is a digit, replacing it with a Z.");

    itemDf$questionCode <-
      gsub("[^a-zA-Z0-9]", "", itemDf$uuid);
    itemDf$questionCode <-
      substr(itemDf$questionCode, nchar(itemDf$questionCode)-20, nchar(itemDf$questionCode));
    itemDf$questionCode <-
      ifelse(grepl("^[0-9]", itemDf$questionCode),
             paste0(
               "Z",
               substr(itemDf$questionCode, nchar(itemDf$questionCode)-19, nchar(itemDf$questionCode))
              ),
             itemDf$questionCode);

  }

  browser();


  ### Convert response option sequences and maybe values to numeric

  soq$response_registration_templates <-
    tryCatch(
      lapply(
        soq$response_registration_templates,  ### Loop through templates; and
        lapply,                               ### then, in each template,
        \(current_respOption) {               ### loop through response options
          current_respOption$response_option_sequence <-
            as.numeric(current_respOption$response_option_sequence);
          current_respOption$response_option_value <-
            ifelse(
                grepl("[^0-9\\. ]", current_respOption$response_option_value),
                current_respOption$response_option_value,
                as.numeric(current_respOption$response_option_value)
              );
            return(current_respOption);
        }
      ),
      error = \(x) stop("Could not convert the `sequence` values to numeric values!")
    );

  ### Sort the response registration templates

  soq$response_registration_templates <-
    lapply(
      soq$response_registration_templates,
      \(current_rrTemplate) {
        current_rrTemplate[
          order(unlist(lapply(
            current_rrTemplate,
            \(x) x$response_option_sequence
          )))
        ]
      }
    );

  ### Create LimeSurvey group

  lsGroup <- limonaid::Group$new(
    group_name = soq$metadata$label,
    language = langAlpha2
  );

  for (uiid in itemDf$uiid) {

    currentQuestion <-
      lsGroup$add_question(
        code = itemDf[uiid, 'questionCode'],
        type = itemDf[uiid, 'questionType'],
        questionTexts = itemDf[uiid, 'question_text']
      );

    current_rrTemplate <-
      soq$response_registration_templates[[
        itemDf[uiid, 'rrTemplateId']
      ]];

    for (current_answerOption in current_rrTemplate) {
      currentQuestion$add_answer_option(
        code = current_answerOption$response_option_value,
        optionTexts = current_answerOption$response_option_content
      );
    }

  }

  if (!is.null(file)) {
    lsGroup$export_to_lsg(file = file);
  }

  return(lsGroup);

}
