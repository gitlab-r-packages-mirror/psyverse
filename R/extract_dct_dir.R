extract_dct_dir <- function(path,
                            extension = "dct",
                            regex,
                            delimiterRegEx = "^---$",
                            ignoreOddDelimiters = FALSE,
                            silent=FALSE) {
  if (missing(regex)) {
    regex <- paste0("^(.*)\\.", extension, "$");
  }

  filelist <- list.files(path,
                         pattern=regex,
                         full.names=TRUE);

  res <- lapply(filelist,
                extract_dct_specs);

  return(structure(res,
                   class="dctRawSpecListSet"));

}
