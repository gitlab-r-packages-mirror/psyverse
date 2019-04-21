#' @rdname generate_dct_templates
#' @export
generate_dct_template <- function(prefix = paste(sample(letters, 4), collapse=""),
                                  output = NULL,
                                  overwrite = FALSE,
                                  createDirs = FALSE,
                                  addComments = TRUE,
                                  stopOnIllegalChars = FALSE) {

  dctId <-
    generate_id(prefix=prefix,
                stopOnIllegalChars=stopOnIllegalChars);

  res <-
    dct_template(dctId = dctId,
                 addComments = addComments);

  if (is.null(output)) {
    return(res);
  } else {
    if (dir.exists(output)) {
      ### User specified a directory, so generate the filename
      output <-
        file.path(output,
                  paste0(dctId, ".dct"));
    }
    if (!dir.exists(dirname(output))) {
      if (createDirs) {
        dir.create(dirname(output),
                   recursive = TRUE);
      } else {
        warning("The `output` argument did not specify an existing directory or a ",
                "file in an existing directory, and `createDirs` was set to `FALSE`, ",
                "so not writing an output file but returning the result.");
        return(res);
      }
    }
    if (file.exists(output)) {
      if (!overwrite) {
        warning("The specified file exists, but `overwrite` was set to `FALSE`, ",
                "so not overwriting the existing file but returning the result.");
        return(res);
      } else {
        warning("The specified file exists, and `overwrite` was set to `TRUE`, ",
                "so overwriting the existing file!");
      }
    }
    con <-
      file(output,
           open="w",
           encoding="UTF-8");
    writeLines(res,
               con);
    close(con);
    return(invisible(res));
  }
}
