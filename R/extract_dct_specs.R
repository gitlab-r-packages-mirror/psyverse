#' Extract DCT specifications from YAML fragments
#'
#' This function takes a file containing YAML fragments and extracts
#' the DCT specifications from them.
#'
#' This function is called by [extract_dct_dir()]; it is normally not
#' necessary to call this function directly.
#'
#' @param file The file containing the YAML fragments.
#' @param text A character vector containing the YAML fragments, where
#' every element should represent one line in the file.
#' @param delimiterRegEx The regular expression used to locate YAML
#' fragments
#' @param ignoreOddDelimiters Whether to throw an error (FALSE) or
#' delete the last delimiter (TRUE) if an odd number of delimiters is
#' encountered.
#' @param silent Whether to be silent (TRUE) or informative (FALSE).
#' @rdname extract_dct_specs
#'
#' @return A list of DCT specifications in an object of class `dtcRawSpecList`.
#' @examples extract_dct_specs(text=unlist(strsplit(dct::example_dct_spec, '\n')));
#'
#' @export
extract_dct_specs <- function(file,
                              text,
                              delimiterRegEx = "^---$",
                              ignoreOddDelimiters = FALSE,
                              silent=FALSE) {

  if (!missing(file)) {
    yamlLineSets <- extract_yaml_fragments(file=file,
                                           delimiterRegEx=delimiterRegEx,
                                           ignoreOddDelimiters=ignoreOddDelimiters,
                                           silent=TRUE);
  } else if (!missing(text)) {
    yamlLineSets <- extract_yaml_fragments(text=text,
                                           delimiterRegEx=delimiterRegEx,
                                           ignoreOddDelimiters=ignoreOddDelimiters,
                                           silent=TRUE);
  } else {
    stop("Provide either a `file` or a `text` to scan!");
  }

  pastedYamlLineSets <- lapply(yamlLineSets,
                               paste,
                               collapse="\n");

  rawSpecs <- lapply(yamlLineSets,
                     yaml::yaml.load);

  rawSpecs <-
    lapply(rawSpecs,
           function(x) {
             if (is.null(x$date)) {
               return(x)
             } else if (nchar(trimws(x$date))==10) {
               x$datetime <- as.numeric(strptime(x$date, "%Y-%m-%d"));
             } else if (nchar(trimws(x$date))==16) {
               x$datetime <- as.numeric(strptime(x$date, "%Y-%m-%d %H:%M"));
             }
             return(x);
           });

  rawSpecs <- lapply(rawSpecs,
                     structure,
                     class='dtcRawSpec');

  return(structure(rawSpecs,
                   class="dtcRawSpecList"));

}
