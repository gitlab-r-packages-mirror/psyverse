#' Import a DCT specification from a spreadsheet
#'
#' This function reads a spreadsheet (from a Google sheet URL or a local file
#' in `.xlsx`, `.csv`, or `.sav` format) and imports the DCT specifications
#' in it.
#'
#' @inheritParams read_spreadsheet
#' @param path The path to save the DCT specifications.
#' @param preventOverwriting Whether to prevent overwriting.
#' @param encoding The encoding to use.
#'
#' @return Invisibly, an object with the worksheets and the DCT objects.
#' @export
dct_from_spreadsheet <-
  function(
    x,
    path = NULL,
    sheet = NULL,
    localBackup = NULL,
    exportGoogleSheet = TRUE,
    xlsxPkg = c("rw_xl", "openxlsx", "XLConnect"),
    preventOverwriting = psyverse::opts$get("preventOverwriting"),
    encoding = psyverse::opts$get("encoding"),
    silent = psyverse::opts$get("silent")
  ) {

    res <- list();

    res$sheets <- read_spreadsheet(
      x = x,
      localBackup = localBackup,
      sheet = sheet,
      exportGoogleSheet = exportGoogleSheet,
      xlsxPkg = xlsxPkg,
      failQuietly = FALSE,
      silent = silent
    );

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
