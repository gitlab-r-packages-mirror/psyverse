#' @rdname generate_dct_templates
#' @export
generate_dct_templates <- function(x,
                                   outputDir = NULL,
                                   createDirs = FALSE,
                                   addComments = FALSE,
                                   stopOnIllegalChars = FALSE) {

  dctIds <-
    generate_ids(x=x,
                 stopOnIllegalChars=stopOnIllegalChars);

  res <-
    lapply(dctIds,
           dct_template,
           addComments = addComments);

  if (is.null(outputDir)) {
    return(res);
  } else {
    outputDir <-
      normalizePath(outputDir,
                    mustWork = FALSE);
    if (!dir.exists(outputDir)) {
      if (createDirs) {
        dir.create(outputDir,
                   recursive = TRUE);
      } else {
        warning("The `outputDir` argument did not specify an existing directory, ",
                "and `createDirs` was set to `FALSE`, so returning the results instead.");
        return(res);
      }
    }
    for (i in seq_along(res)) {
      con <-
        file(file.path(outputDir,
                       paste0(dctIds[i], ".dct")),
             open="w",
             encoding="UTF-8");
      writeLines(res[[i]],
                 con);
      close(con);
    }
  }

}
