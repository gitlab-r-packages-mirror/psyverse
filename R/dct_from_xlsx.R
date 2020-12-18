#' Title
#'
#' @param xlsx
#' @param path
#' @param preventOverwriting
#' @param encoding
#'
#' @return
#' @export
#'
#' @examples
dct_from_xlsx <-
  function(
    xlsx,
    path = NULL,
    preventOverwriting = psyverse::opts$get("preventOverwriting"),
    encoding = psyverse::opts$get("encoding")
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
