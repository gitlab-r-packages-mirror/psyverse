#' Import a DCT specification from an Excel (.xlsx) spreadsheet
#'
#' @param xlsx The path to the spreadsheet, its key)].
#' @param path The path to save the DCT specifications.
#' @param preventOverwriting Whether to prevent overwriting.
#' @param encoding The encoding to use.
#' @param silent Whether to be silent or chatty.
#'
#' @return Invisibly, an object with the worksheets and the DCT objects.
#' @export
dct_from_xlsx <-
  function(
    xlsx,
    path = NULL,
    preventOverwriting = psyverse::opts$get("preventOverwriting"),
    encoding = psyverse::opts$get("encoding"),
    silent = psyverse::opts$get("silent")
  ) {

  if (!file.exists(xlsx)) {
    stop("File `", xlsx, "` does not exist!");
  }

  wb <- openxlsx::loadWorkbook(xlsx);
  sheets <- openxlsx::sheets(wb);

  res <- list(sheets = list());
  for (currentSheet in sheets) {

    ### Read the worksheet for this language
    res$sheets[[currentSheet]] <-
      as.data.frame(
        openxlsx::read.xlsx(
          wb,
          sheet = currentSheet
        ),
        stringsAsFactors = FALSE
      );

  }

  res$dcts <-
    lapply(
      names(res$sheets),
      function(sheet) {
        if (!silent) {
          cat0("Converting DCT sheet '", sheet, "' to a DCT object.\n");
        }
        return(dct_sheet_to_dct(res$sheets[[sheet]]));
      }
    );

  if ((!is.null(path)) && (dir.exists(path))) {
    for (i in res$dcts) {
      save_to_yaml(x = i,
                   file = file.path(path,
                                    paste0(i$id, ".dct")),
                   preventOverwriting = preventOverwriting,
                   encoding = encoding);
    }
  }

  return(invisible(res));

}
