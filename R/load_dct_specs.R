#' Load DCT specifications from a file or multiple files
#'
#' These function load DCT specifications from the YAML fragments
#' in one (`load_dct_specs`) or multiple files (`load_dct_dir`).
#'
#' `load_dct_dir` simply identifies all files and then calls
#' `load_dct_specs` for each of them. `load_dct_specs` loads the
#' YAML fragments containing the DCT specifications using
#' [yum::load_yaml_fragments()] and then parses the DCT
#' specifications into a visual representation as a
#' [DiagrammeR::DiagrammeR] graph and Markdown documents with
#' the instructions for creating measurement instruments or
#' manipulations, and for coding measurement instruments,
#' manipulations, or aspects of a construct.
#'
#' @param file,text Either specify a `file` to read with encoding `encoding`, which will
#' then be read using [base::readLines()] (if specified, takes precedence over `text`);
#' or specify a `text` to process, which should be a character vectors where every
#' element is a line of the original source (like provided [base::readLines()]);
#' although if a character vector of one element *but* including at least one
#' newline character (`\\n`) is provided as `text`, this is split at the newline
#' characters using [base::strsplit()].
#' @param path The path containing the files to read.
#' @param extension The extension of the files to read; files with other extensions will
#' be ignored. Multiple extensions can be separated by a pipe (`|`).
#' @param regex Instead of specifing an extension, it's also possible to specify a regular
#' expression; only files matching this regular expression are read. If specified, `regex`
#' takes precedece over `extension`,
#' @param recursive Whether to also process subdirectories (`TRUE`)
#' or not (`FALSE`).
#' @param delimiterRegEx The regular expression used to locate YAML
#' fragments
#' @param dctContainer The container of the DCT specifications in the YAML
#' fragments. Because only DCT specifications are read that are stored in
#' this container, the files can contain YAML fragments with other data, too,
#' without interfering with the parsing of the DCT specifications.
#' @param arrowDirection The direction of the arrows in the visual representation
#' of the distributed construct taxonomy; either `forward`, `back`, `both`, or `none`.
#' @param ignoreOddDelimiters Whether to throw an error (FALSE) or
#' delete the last delimiter (TRUE) if an odd number of delimiters is
#' encountered.
#' @param encoding The encoding to use when calling [readLines()]. Set to
#' NULL to let [readLines()] guess.
#' @param silent Whether to be silent (TRUE) or informative (FALSE).
#' @param x The parsed `parsed_dct` object.
#' @param ... Any other arguments are passed to the print command.
#'
#' @rdname load_dct_specs
#' @aliases load_dct_specs load_dct_dir print.dct_specs plot.dct_specs
#'
#' @return An object with the [DiagrammeR::DiagrammeR] graph stored
#' in `output$graph` and the instructions in `output$instr`.
#'
#' @examples
#' load_dct_specs(text=example_dct_spec);
#'
#' \dontrun{
#' dct::load_dct_dir(path="A:/some/path");
#' }
#'
#' @export
load_dct_specs <- function(file,
                           text,
                           delimiterRegEx = "^---$",
                           dctContainer = "dct",
                           arrowDirection = "forward",
                           ignoreOddDelimiters = FALSE,
                           encoding="UTF-8",
                           silent=TRUE) {

  ###--------------------------------------------------------------------------
  ### Load the YAML fragments containing the DCT specifications
  ###--------------------------------------------------------------------------

  if (!missing(file)) {
    dctSpecs <- yum::load_yaml_fragments(file=file,
                                         delimiterRegEx=delimiterRegEx,
                                         select=dctContainer,
                                         ignoreOddDelimiters=ignoreOddDelimiters,
                                         encoding=encoding,
                                         silent=silent);
  } else if (!missing(text)) {
    dctSpecs <- yum::load_yaml_fragments(text=text,
                                         delimiterRegEx=delimiterRegEx,
                                         select=dctContainer,
                                         ignoreOddDelimiters=ignoreOddDelimiters,
                                         encoding=encoding,
                                         silent=silent);
  } else {
    stop("Specify either `file` or `text` to load.");
  }

  ###--------------------------------------------------------------------------
  ### Parse DCT specifications and return result
  ###--------------------------------------------------------------------------

  res <-
    parse_dct_specs(dctSpecs,
                    arrowDirection=arrowDirection);

  return(res);

}

#' @rdname load_dct_specs
#' @method print dct_specs
#' @export
print.dct_specs <- function(x, ...) {
  cat("Processed", length(x$intermediate$dctSpecs),
      "specifications, containing", nrow(x$intermediate$nodes),
      "distinct constructs. Graph and instructions for",
      "developing measurement instruments and manipulations and for",
      "coding measurement instruments, manipulations, and aspects",
      "are now available in the returned object, if you stored it.");
  invisible(x);
}

#' @rdname load_dct_specs
#' @method plot dct_specs
#' @export
plot.dct_specs <- function(x, ...) {
  DiagrammeR::render_graph(x$output$basic_graph);
}


