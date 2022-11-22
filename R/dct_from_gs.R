#' #' Import a DCT specification from a Google Sheets spreadsheet
#' #'
#' #' @param gs The URL to the Google Sheets spreadsheet, its key, or the
#' #' object resulting from a call to [googlesheets4::gs4_get()].
#' #' @param sheets `TRUE` to read all worksheets, or a vector with the
#' #' indices or names of the worksheets to read.
#' #' @param path The path to save the DCT specifications.
#' #' @param throttleSeconds The number of seconds to wait between calls to the
#' #' Google Sheets API (to prevent being locked out).
#' #' @param localBackup If pointing to a filename in an existing path,
#' #' the spreadsheet will be saved as `xlsx` file using
#' #' the [openxlsx::openxlsx] package.
#' #' @param preventOverwriting Whether to prevent overwriting.
#' #' @param encoding The encoding to use.
#' #'
#' #' @return Invisibly, an object with the worksheets and the DCT objects.
#' #' @export
#' dct_from_gs <-
#'   function(
#'     gs,
#'     sheets = TRUE,
#'     path = NULL,
#'     localBackup = NULL,
#'     throttleSeconds = psyverse::opts$get("throttleSeconds"),
#'     preventOverwriting = psyverse::opts$get("preventOverwriting"),
#'     encoding = psyverse::opts$get("encoding")
#'   ) {
#'
#'   if (!requireNamespace('googlesheets4')) {
#'     stop("To import DCT specifications from Google Sheets, you ",
#'          "need to have the {googlesheets4} package installed. You ",
#'          "can install it with:\n\n  ",
#'          "install.packages('googlesheets4');\n");
#'   }
#'
#'   googlesheets4::gs4_deauth();
#'
#'   if (!("googlesheets4_spreadsheet" %in% class(gs))) {
#'     gs <- googlesheets4::gs4_get(gs);
#'   }
#'
#'   # if (is.character(gs) && (length(gs) == 1) && grepl("^http", gs)) {
#'   #   gs <-
#'   #     googlesheets::gs_url(
#'   #       gs,
#'   #       verbose = FALSE
#'   #     );
#'   # } else if (is.character(gs) && (nchar(gs) == 44)) {
#'   #   gs <-
#'   #     googlesheets4::gs_key(
#'   #       gs,
#'   #       verbose = FALSE
#'   #     );
#'   # } else if (!("googlesheet" %in% class(gs))) {
#'   #   stop("As `gs`, pass either a `googlesheets` object, or a Google Sheets URL.");
#'   # }
#'
#'   if (isTRUE(sheets)) {
#'     #sheets <- googlesheets::gs_ws_ls(gs);
#'     sheets <- googlesheets4::sheet_names(gs);
#'   }
#'
#'   if (is.null(localBackup)) {
#'     makeBackup <- FALSE;
#'   } else if (dir.exists(dirname(localBackup))) {
#'     backupPath <- dirname(localBackup);
#'     makeBackup <- TRUE;
#'   } else {
#'     makeBackup <- FALSE;
#'   }
#'
#'   if (makeBackup) {
#'
#'     if (!requireNamespace('openxlsx')) {
#'       stop("To import DCT specifications from an Excel spreadsheet, you ",
#'            "need to have the {openxlsx} package installed. You ",
#'            "can install it with:\n\n  ",
#'            "install.packages('openxlsx');\n");
#'     }
#'
#'     ### Create workbook to back this up to an .xlsx file
#'     wb <- openxlsx::createWorkbook(creator = paste0("psyverse ",
#'                                                     utils::packageVersion("psyverse")),
#'                                    title = "DCT specifications",
#'                                    subject = NULL,
#'                                    category = NULL);
#'   }
#'
#'   res <- list(sheets = list());
#'   for (currentSheet in sheets) {
#'
#'     ### Read the worksheet for this language
#'     res$sheets[[currentSheet]] <-
#'       as.data.frame(
#'         # googlesheets::gs_read(
#'         #   gs,
#'         #   ws = currentSheet,
#'         #   verbose = FALSE
#'         # ),
#'         googlesheets4::read_sheet(
#'           gs,
#'           sheet = currentSheet
#'         ),
#'         stringsAsFactors = FALSE
#'       );
#'
#'     if (makeBackup) {
#'       currentSheetName <-
#'         ifelse(nchar(currentSheet) < 30,
#'                currentSheet,
#'                substring(currentSheet, 1, 30));
#'       ### Create worksheet and add data
#'       openxlsx::addWorksheet(wb,
#'                              sheetName = currentSheetName);
#'       openxlsx::writeData(wb,
#'                           sheet = currentSheetName,
#'                           x = res$sheets[[currentSheet]],
#'                           startCol = "A",
#'                           startRow = 1);
#'     }
#'
#'     Sys.sleep(throttleSeconds);
#'
#'   }
#'
#'   if (makeBackup) {
#'     if ((!file.exists(localBackup)) || (!preventOverwriting)) {
#'       ### Write workbook to disk
#'       openxlsx::saveWorkbook(
#'         wb,
#'         file = localBackup,
#'         overwrite = TRUE
#'       );
#'     } else {
#'       cat("Local backup file '", localBackup,
#'           "' exists, and preventOverwriting ",
#'           "is set to TRUE, so did not store local backup (use ",
#'           "'preventOverwriting=FALSE' to no longer prevent overwriting).");
#'     }
#'   }
#'
#'   res$dcts <-
#'     lapply(
#'       res$sheets,
#'       dct_sheet_to_dct
#'     );
#'
#'   if ((!is.null(path)) && (dir.exists(path))) {
#'     for (i in res$dcts) {
#'       save_to_yaml(x = i,
#'                    file = file.path(path,
#'                                     paste0(i$id, ".dct")),
#'                    preventOverwriting = preventOverwriting,
#'                    encoding = encoding);
#'     }
#'   }
#'
#'   return(invisible(res));
#'
#' }
