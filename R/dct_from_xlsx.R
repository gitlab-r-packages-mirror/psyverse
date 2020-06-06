dct_from_xlsx <-
  function(
    xlsx,
    sheets = TRUE,
    path = NULL,
    preventOverwriting = psyverse::opts$get("preventOverwriting"),
    encoding = psyverse::opts$get("encoding")
  ) {

  if (isTRUE(sheets)) {
    sheets <- googlesheets::gs_ws_ls(gs);
  }

  if (dir.exists(dirname(localBackup))) {
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
    if ((!file.exists(localBackup)) || preventOverwriting) {
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

  return(invisible(res));

}
