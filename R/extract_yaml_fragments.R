extract_yaml_fragments <- function(file,
                                   delimiterRegEx = "^---$",
                                   ignoreOddDelimiters = FALSE,
                                   silent=TRUE) {
  allLines <- readLines(file);
  yamlFragments <- grep(delimiterRegEx,
                        allLines);
  if (!ufs::is.even(length(yamlFragments))) {
    if (ignoreOddDelimiters) {
      yamlFragments <-
        yamlFragments[-length(yamlFragments)];
    } else {
      stop("Extracted an uneven number of lines with specifications ",
           "(the regular expression for the specification ",
           "delimiter that was specified was '", delimiterRegEx,
           "'). To ignore the last delimiter, specify ",
           "'ignoreOddDelimiters=TRUE'.");
    }
  }

  yamlFragmentIndices <- seq_along(yamlFragments);

  indexSets <- purrr::map2(.x=yamlFragments[ufs::is.odd(yamlFragmentIndices)],
                           .y=yamlFragments[ufs::is.even(yamlFragmentIndices)],
                           .f=`:`);

  return(lapply(indexSets,
                function(i, x=allLines) {
                  return(x[i]);
                }));

}
