dct_sheet_to_dct <- function(dct_sheet) {

  if (!("data.frame" %in% class(dct_sheet))) {
    stop("You must pass a data frame as `", dct_sheet, "`; ",
         "instead, you passed an object of class(es) ",
         vecTxtQ(class(dct_sheet)), ".");
  }

  fieldCol <- psyverse::opts$get("dct_sheet_fieldCol");
  contentCol <- psyverse::opts$get("dct_sheet_contentCol");

  if (!all(c(fieldCol, contentCol) %in% names(dct_sheet))) {
    stop("The columns with the field names (`", fieldCol, "`) and with the ",
         "field contents (`", contentCol, "`) do not both exist in the ",
         "data frame you provided as `dct_sheet`.");
  }

  dctFields <-
    c(
      "prefix",
      "label",
      "definition",
      "measure_dev",
      "measure_code",
      "aspect_dev",
      "aspect_code"
    );

  dct <- lapply(
    dctFields,
    function(fieldName) {
      res <- dct_sheet[dct_sheet[, fieldCol] == fieldName, contentCol];
      if (all(is.na(res))) {
        res <- "";
      }
      if (length(res) < 1) {
        stop("The contents of field `", fieldName, "` have length ",
             length(res), "!");
      } else if (length(res) > 1) {
        stop("The contents of field `", fieldName, "` have length ",
             length(res), "!");
      } else {
        return(res);
      }
    });
  names(dct) <-
    dctFields;

  return(
    dct_object(
      version = as.character(packageVersion("psyverse")),
      prefix = dct$prefix,
      label = dct$label,
      date = as.character(Sys.Date()),
      ancestry = "",
      retires = "",
      definition = list(definition = dct$definition),
      measure_dev = list(instruction = dct$measure_dev),
      measure_code = list(instruction = dct$measure_code),
      aspect_dev = list(instruction = dct$aspect_dev),
      aspect_code = list(instruction = dct$aspect_code)
    )
  );

}
