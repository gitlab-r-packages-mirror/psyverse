#' @rdname load_dct_specs
#' @export
load_dct_dir <- function(path,
                         recursive = TRUE,
                         extension = "rock|dct",
                         regex,
                         dctContainer = "dct",
                         arrowDirection = "forward",
                         delimiterRegEx = "^---$",
                         ignoreOddDelimiters = FALSE,
                         encoding="UTF-8",
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

  dctSpecList <-
    yum::load_and_simplify_dir(path=path,
    #yum::load_yaml_dir(path=path,
                       recursive=recursive,
                       fileRegexes = regex,
                       select=dctContainer,
                       delimiterRegEx = delimiterRegEx,
                       ignoreOddDelimiters = ignoreOddDelimiters,
                       encoding = encoding,
                       silent=silent);

  ### Remove 'file' level
  # dctSpecs <-
  #   unlist(dctSpecList,
  #          recursive=FALSE);

  ###--------------------------------------------------------------------------
  ### Parse DCT specifications and return result
  ###--------------------------------------------------------------------------

  res <-
    parse_dct_specs(dctSpecs,
                    arrowDirection=arrowDirection);

  return(res);

}
