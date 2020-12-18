#' Create a DCT object from a DCT sheet
#'
#' @param dct_sheet A dataframe containing a DCT specification.
#'
#' @return A DCT created by [dct_object()].
#' @export
#'
#' @examples
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

  dctFields_required <-
    c(
      "label",
      "definition",
      "measure_dev",
      "measure_code",
      "aspect_dev",
      "aspect_code"
    );
  dctFields_optional <-
    c(
      "ancestry",
      "retires",
      "rel"
    );

  dct <- lapply(
    dctFields_required,
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
    dctFields_required;

  if ("id" %in% dct_sheet[, fieldCol]) {
    dct$id <- dct_sheet[dct_sheet[, fieldCol] == "id", contentCol];
    if ((is.na(dct$id)) || nchar(trimws(dct$id)) == 0) {
      dct$id <- NULL;
    }
  } else {
    dct$id <- NULL;
  }
  if ("prefix" %in% dct_sheet[, fieldCol]) {
    dct$prefix <- dct_sheet[dct_sheet[, fieldCol] == "prefix", contentCol];
  } else {
    dct$prefix <- NULL;
  }
  if ((!is.null(dct$id) & !is.null(dct$prefix)) &&
      (!grepl(dct$prefix, dct$id, fixed=TRUE))) {
    stop("The DCT sheet contained both a specified full identifier (`id` = `",
         dct$id, "`) and an identifier prefix (`prefix` = `", dct$prefix,
         "`), but the prefix is not contained within the identifier!");
  }

  dct <-
    c(
      dct,
      setNames(
        lapply(
          dctFields_optional,
          function(fieldName) {
            res <- dct_sheet[dct_sheet[, fieldCol] == fieldName, contentCol];
            if ((all(is.na(res))) || (length(res) < 1)) {
              res <- "";
            } else if (length(res) > 1) {
              res <- paste0(
                res,
                collapse = " ||| "
              );
            }
            return(res);
          }),
        nm = dctFields_optional
      )
    );

  dct$id <-
    gsub("\\s", "", dct$id);

  dct$label <-
    gsub("^(\\s)", "", dct$label);
  dct$label <-
    gsub("(\\s)$", "", dct$label);

  res <-
    dct_object(
      version = as.character(packageVersion("psyverse")),
      prefix = dct$prefix,
      id = dct$id,
      label = dct$label,
      date = as.character(Sys.Date()),
      ancestry = "",
      retires = "",
      definition = list(definition = dct$definition),
      measure_dev = list(instruction = dct$measure_dev),
      measure_code = list(instruction = dct$measure_code),
      aspect_dev = list(instruction = dct$aspect_dev),
      aspect_code = list(instruction = dct$aspect_code)
    );

  return(
    res
  );

}
