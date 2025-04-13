#' @rdname load_dct_specs
#' @export
load_dct_dir <- function(path,
                         recursive = TRUE,
                         extension = "\\.rock|\\.dct\\.yaml|\\.yaml|\\.yml",
                         regex,
                         dctContainer = "dct",
                         headingLevel = 2,
                         delimiterRegEx = "^---$",
                         ignoreOddDelimiters = FALSE,
                         encoding="UTF-8",
                         sortDecreasing = FALSE,
                         silent=TRUE) {

  if (!dir.exists(path)) {
    stop("Directory '",
         path,
         "' does not exist!");
  }

  if (missing(regex)) {
    regex <- paste0("^(.*)\\.", extension, "$");
  }

  ###--------------------------------------------------------------------------
  ### Load the YAML fragments containing the DCT specifications
  ###--------------------------------------------------------------------------

  dctSpecs <-
    yum::load_and_simplify_dir(
      path = path,
      recursive = recursive,
      fileRegexes = regex,
      select = dctContainer,
      delimiterRegEx = delimiterRegEx,
      ignoreOddDelimiters = ignoreOddDelimiters,
      encoding = encoding,
      silent = silent
    );

  ###--------------------------------------------------------------------------
  ### Parse DCT specifications and return result
  ###--------------------------------------------------------------------------

  dctSpecs <- lapply(
    dctSpecs,
    function(x) {
      class(x) <- c("psyverse_dct", class(x));
      return(x);
    }
  );

  res <-
    parse_dct_specs(dctSpecs,
                    headingLevel=headingLevel,
                    sortDecreasing=sortDecreasing);

  return(res);

}
