#' Extract DCT specifications from all files in a directory
#'
#' This function processes all DCT specifications in a directory.
#'
#' This function is called by [process_dir()]; it is normally not
#' necessary to call this function directly.
#'
#' @param path The path from where to read the DCT files.
#' @param extension The extension of the file; convenient alternative to
#'   specifying a `regex`.
#' @param regex A regular expression; when provided, overrides the `extension`
#'   argument to guide file selection in the `path`.
#' @param delimiterRegEx The regular expression specifying how the YAML fragments
#'   specifying the constructs are delimited. Should normally never be changed.
#' @param ignoreOddDelimiters Whether to ignore a final odd delimiter, if
#'   encountered.
#' @param silent Whether to provide information on progress.
#'
#' @return An object of class `dctRawSpecListSet` for processing by [parse_dct_specs()].
#' @examples \dontrun{extract_dct_dir("A:/path/to/some/directory");
#' }
#' @export
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

  if (length(filelist) == 0) {
    stop(glue::glue("No files in directory '{path}' matched ",
                    "regular expression {regex}."));
  }

  res <- lapply(filelist,
                extract_dct_specs);

  return(structure(res,
                   class="dctRawSpecListSet"));

}
