#' Process a directory with DCT files
#'
#' This function locates and parses all DCT files in a directory. These
#' files are then parsed into a DiagrammeR graph as well as lists of
#' instructions for development and coding.
#'
#' @param path The path from where to read the DCT files.
#' @param extension The extension of the file; convenient alternative to
#'   specifying a `regex`.
#' @param arrowDirection The direction of the arrows in the visual representation
#' of the distributed construct taxonomy; either `forward`, `back`, `both`, or `none`.
#' @param regex A regular expression; when provided, overrides the `extension`
#'   argument to guide file selection in the `path`.
#' @param delimiterRegEx The regular expression specifying how the YAML fragments
#'   specifying the constructs are delimited. Should normally never be changed.
#' @param dctContainer The container of the DCT specifications in the YAML
#'   fragments. Because only DCT specifications are read that are stored in
#'   this container, the files can contain YAML fragments with other data, too,
#'   without interfering with the parsing of the DCT specifications.
#' @param ignoreOddDelimiters Whether to ignore a final odd delimiter, if
#'   encountered.
#' @param silent Whether to provide information on progress.
#'
#' @return An object with the [DiagrammeR::DiagrammeR] graph stored
#' in `output$graph` and the instructions in `output$instr` (the result of
#' a call to [parse_dct_specs()] on all matching files).
#' @examples \dontrun{process_dir("A:/path/to/some/directory");
#' }
#' @export
process_dir <- function(path,
                        extension = "dct",
                        arrowDirection = "forward",
                        regex,
                        delimiterRegEx = "^---$",
                        dctContainer = "dct",
                        ignoreOddDelimiters = FALSE,
                        silent=FALSE) {

  rawSpecs <- extract_dct_dir(path,
                              extension = "dct",
                              regex,
                              delimiterRegEx = "^---$",
                              dctContainer=dctContainer,
                              ignoreOddDelimiters = FALSE,
                              silent=FALSE);

  return(parse_dct_specs(rawSpecs));

}
