#' Create a DCT object
#'
#' @param version The version of the DCT specification format (normally the version
#' of the `psyverse` package).
#' @param id The Unique Construct Identifier (UCID); if not provided,
#' this is created using the `prefix`.
#' @param prefix The prefix to use to construct the Unique Construct Identifier
#' (UCID); ignored i `id` is provided.
#' @param label The human-readable label for the construct.
#' @param date The date at which the construct was created.
#' @param dct_version The version of the DCT specification. This can optionally
#' be used to manage consecutive DCT versions.
#' @param ancestry The DCT specification or specifications that this DCT was
#' based on.
#' @param retires The DCT specification or specifications that this DCT renders
#' obsolete (note that this doesn't mean anything in itself; `psyverse` does not
#' enforce this automatically, nor does PsyCoRe, without configuration).
#' @param definition The definition of the construct. This has to be comprehensive,
#' detailed, accurate, and clearly delineate the relevant aspects of the human
#' psychology.
#' @param measure_dev Instructions for developing measurement instruments that
#' measure this construct.
#' @param measure_code Instructions for coding measurement instruments (e.g. in
#' systematic reviews) as measurement instruments that measure this construct.
#' Note that explicitly defining boundary conditions often helps, for example by
#' explaining the features that coders should look for to distinguish this
#' construct from closely related constructs (ideally linking to those other
#' constructs using the `dct::UCID` notations).
#' @param aspect_dev Instructions for eliciting construct content. Note that
#' this is not sensible for all constructs; some may be defined at a very
#' general level, rendering their content insufficiently specific to discuss
#' or describe.
#' @param aspect_code Instructions for coding construct content (i.e. aspects).
#' Note that explicitly defining boundary conditions often helps, for example by
#' explaining the features that coders should look for to distinguish this
#' construct from closely related constructs (ideally linking to those other
#' constructs using the `dct::UCID` notations).
#' @param rel Relationships with other constructs.
#'
#' @return The DCT object.
#' @export
#' @examples exampleDCT <-
#'   psyverse::dct_object(
#'     prefix = "exampleConstruct",
#'     label = "An example construct",
#'     definition = "The definition goes here",
#'     measure_dev = "Here you can explain how to measure the construct"
#'   );
dct_object <-
  function(
    version = as.character(utils::packageVersion("psyverse")),
    id = NULL,
    prefix = paste(sample(letters, 4), collapse=""),
    label = "",
    date = as.character(Sys.Date()),
    dct_version = "1",
    ancestry = "",
    retires = "",
    definition = "",
    measure_dev = "",
    measure_code = "",
    aspect_dev = "",
    aspect_code = "",
    rel = NULL
  ) {

  if (is.null(id)) {
    id <- psyverse::generate_id(prefix=prefix);
  } else if (length(id) == 0) {
    id <- psyverse::generate_id(prefix=prefix);
  } else if (is.na(id)) {
    id <- psyverse::generate_id(prefix=prefix);
  } else if (nchar(id) < 5) {
    if (nchar(id) > 0) {
      warning(cat("An id of too few characters (", nchar(id),
                  ") was passed and will be overwritten!", sep=""));
    }
    id <- psyverse::generate_id(prefix=prefix);
  }

  res <-
    list(
      version = version,
      id = id,
      label = label,
      date = date,
      dct_version = dct_version,
      ancestry = ancestry,
      retires = retires,
      definition = nest_in_list(definition,  nestIn = "definition"),
      measure_dev = nest_in_list(measure_dev,  nestIn = "instruction"),
      measure_code = nest_in_list(measure_code, nestIn = "instruction"),
      aspect_dev = nest_in_list(aspect_dev,   nestIn = "instruction"),
      aspect_code = nest_in_list(aspect_code,  nestIn = "instruction"),
      rel = rel
    );

  class(res) <-
    "psyverse_dct";

  return(res);

}
