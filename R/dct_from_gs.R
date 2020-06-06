#' Import a DCT specification from a Google Sheets spreadsheet
#'
#' @param gs The URL to the Google Sheets spreadsheet.
#' @param sheets `TRUE` to read all worksheets, or a vector with the
#' indices or names of the worksheets to read.
#' @param path The path to save the DCT specifications.
#' @param localBackup If pointing to a filename in an existing path,
#' the spreadsheet will be saved as `xlsx` file using
#' the [openxlsx::openxlsx] package.
#' @param preventOverwriting Whether to prevent overwriting.
#' @param encoding The encoding to use.
#'
#' @return Invisibly, an object with the worksheets and the DCT objects.
#' @export
#'
#' @examples
dct_from_gs <-
  function(
    gs,
    sheets = TRUE,
    path = NULL,
    localBackup = NULL,
    preventOverwriting = psyverse::opts$get("preventOverwriting"),
    encoding = psyverse::opts$get("encoding")
  ) {

  if (is.character(gs) && (length(gs) == 1) && grepl("^http", gs)) {
    gs <-
      googlesheets::gs_url(
        gs,
        verbose = FALSE
      );
  } else if (is.character(gs) && (nchar(gs) == 44)) {
    gs <-
      googlesheets::gs_key(
        gs,
        verbose = FALSE
      );
  } else if (!("googlesheet" %in% class(gs))) {
    stop("As `gs`, pass either a `googlesheets` object, or a Google Sheets URL.");
  }

  if (isTRUE(sheets)) {
    sheets <- googlesheets::gs_ws_ls(gs);
  }

  if (is.null(localBackup)) {
    makeBackup <- FALSE;
  } else if (dir.exists(dirname(localBackup))) {
    backupPath <- dirname(localBackup);
    makeBackup <- TRUE;
  } else {
    makeBackup <- FALSE;
  }

  if (makeBackup) {
    ### Create workbook to back this up to an .xlsx file
    wb <- openxlsx::createWorkbook(creator = paste0("psyverse ",
                                                    packageVersion("psyverse")),
                                   title = "DCT specifications",
                                   subject = NULL,
                                   category = NULL);
  }

  res <- list(sheets = list());
  for (currentSheet in sheets) {

    ### Read the worksheet for this language
    res$sheets[[currentSheet]] <-
      as.data.frame(
        googlesheets::gs_read(
          gs,
          ws = currentSheet,
          verbose = FALSE
        ),
        stringsAsFactors = FALSE
      );

    if (makeBackup) {
      ### Create worksheet and add data
      openxlsx::addWorksheet(wb,
                             sheetName = currentSheet);
      openxlsx::writeData(wb,
                          sheet = currentSheet,
                          x = res$sheets[[currentSheet]],
                          startCol = "A",
                          startRow = 1);
    }
  }

  if (makeBackup) {
    if ((!file.exists(localBackup)) || (!preventOverwriting)) {
      ### Write workbook to disk
      openxlsx::saveWorkbook(
        wb,
        file = localBackup,
        overwrite = TRUE
      );
    } else {
      cat("Local backup file '", localBackup,
          "' exists, and preventOverwriting ",
          "is not set to TRUE, so did not store local backup.");
    }
  }

  res$dcts <-
    lapply(
      res$sheets,
      dct_sheet_to_dct
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
