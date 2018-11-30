#' Process a directory with DCT files
#'
#' This function locates and parses all DCT files in a directory. These
#' files are then parsed into a DiagrammeR graph as well as lists of
#' instructions for development and coding.
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
#' @return The result of a call to `parse_dct_specs` on all matching files.
#' @export
#'
#' @examples
process_dir <- function(path,
                        extension = "dct",
                        regex,
                        delimiterRegEx = "^---$",
                        ignoreOddDelimiters = FALSE,
                        silent=FALSE) {

  rawSpecs <- extract_dct_dir(path,
                              extension = "dct",
                              regex,
                              delimiterRegEx = "^---$",
                              ignoreOddDelimiters = FALSE,
                              silent=FALSE);

  return(parse_dct_specs(rawSpecs));

}


# z <- process_dir("B:/Data/R/tmp");
#
# DiagrammeR::render_graph(z$output$graph);
#
# ufs::cat0(paste0(z$output$instr$measure_code, collape="\n"));
