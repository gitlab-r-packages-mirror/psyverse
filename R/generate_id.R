generate_id <- function(prefix = paste(sample(letters, 4), collapse=""),
                        stopOnIllegalChars = FALSE) {

  if (length(prefix) > 1) {
    warning("Use `generate_ids` to generate multiple ids at once; ",
            "calling it for you now and returning the result.");
    return(generate_ids(prefix,
                        stopOnIllegalChars=stopOnIllegalChars));
  }

  if ((grepl("[^a-zA-Z_]+",
             prefix))) {
    if (stopOnIllegalChars) {
      stop("The specified prefix contains illegal characters, and argument ",
           "`stopOnIllegalChars` is set to TRUE, so I'm stopping.");
    } else {
      warning("The specified prefix contains illegal characters, and argument ",
              "`stopOnIllegalChars` is set to FALSE, so I'm removing them.");
      prefix <-
        gsub("[^a-zA-Z_]+",
             "",
             prefix);
    }
  };

  timeNrString <- gsub('\\.', '', format(Sys.time(), "%y%m%d%H%M%OS2"));

  res <- numericToBase30(as.numeric(timeNrString));
  return(paste0(prefix, "_", res));
}
