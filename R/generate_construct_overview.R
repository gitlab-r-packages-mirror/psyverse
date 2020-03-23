#' Generate construct overviews and instruction overviews
#'
#' These functions use a DCT specification to generate a
#' construct overview or an instruction overview.
#'
#' @param dctSpec The DCT specification, as resulting from a call
#' to [load_dct_specs()] or [load_dct_dir()].
#' @param include Which elements to include in the construct overview.
#' @param dctSpecDf The DCT specification dataframer, as produced
#' by a call to [load_dct_specs()] or [load_dct_dir()], and stored within
#' the resulting object.
#' @param type For instruction overviews, the type of instruction
#' to generate can be specified: must be
#' @param headingLevel The level of the heading in the Markdown output
#' that is produces.
#' @param hyperlink_ucids The type of hyperlinks to generate; must be
#' a valid string. Currently, if the value is"`Markdown`" or "`HTML`",
#' hyperlinks in the corresponding formats are produced, and if it is
#' "`none`" (or, actually any other string value), nothing is produced.
#' @param urlPrefix The prefix to insert before the URL in the produced
#' hyperlink. The default, "`#`", results in a link to an
#' anchor (an HTML `a` element) on the current page.
#'
#' @return A character string with the overview.
#' @rdname overview_generation
#' @alias generate_construct_overview
#' generate_instruction_overview
#' generate_definitions_overview
#' @export
#'
#' @examples ### Add example
generate_construct_overview <- function(dctSpec,
                                        include = c("definition",
                                                    "measure_dev",
                                                    "measure_code",
                                                    "manipulate_dev",
                                                    "manipulate_code",
                                                    "aspect_dev",
                                                    "aspect_code",
                                                    "rel"),
                                        headingLevel = 3,
                                        hyperlink_ucids = "Markdown",
                                        urlPrefix = "#") {

  if ("dct_specs" %in% class(dctSpec)) {
    return(
      stats::setNames(
        lapply(dctSpec$intermediate$dctSpec,
               function(x) {
                 return(
                   generate_construct_overview(
                     x,
                     include = include,
                     headingLevel = headingLevel,
                     hyperlink_ucids = hyperlink_ucids,
                     urlPrefix = urlPrefix
                   )
                 );
               }),
        nm = names(dctSpec$intermediate$dctSpec)));
  }

  instrPrepFnc <- function(x) {
    if (is.null(x)) {
      res <- "*Not specified*";
    } else {
      res <-
        gsub("\\n", "\n\n", x);
      ### Replace links to DCTs with hyperlinks
      if (hyperlink_ucids == "Markdown") {
        res <- hyperlink_ucids(res,
                               urlPrefix = urlPrefix);
      } else if (hyperlink_ucids == "HTML") {
        res <- hyperlink_ucids(res,
                               replacement = paste0('<a href="',
                                                    urlPrefix,
                                                    '\\1">dct:\\1</a>'));
      }

    }
    return(res);
  }

  res <-
    c("",
      paste0(repStr("#", headingLevel), " ", dctSpec$label, " {#", dctSpec$id, "}"),
      "",
      format(Sys.time(), '*This overview was generated on %Y-%m-%d at %H:%M:%S %Z (GMT%z)*'),
      "",
      paste0("This Decentralized Construct Taxonomy specification was authored at ",
             ifelse(is.null(dctSpec$date),
                    "an unknown date (i.e. this was not specified in the DCT specification)",
                    dctSpec$date),
             "."),
      "",
      paste0("Unique Construct Identifier (UCID): ", dctSpec$id),
      "");
  if ("definition" %in% include) {
    res <-
      c(res,
        paste0(repStr("#", headingLevel+1), " Definition"),
        "",
        "<a style=\"float:right\" class=\"btn btn-light\" onclick=\"$(this).parent().next('.toggleable').toggle()\">Show / Hide</a>",
        "<div class='toggleable'>",
        instrPrepFnc(dctSpec$definition$definition),
        "</div>",
        "");
  }
  if ("measure_dev" %in% include) {
    res <-
      c(res,
        paste0(repStr("#", headingLevel+1), " Instruction for developing measurement instruments"),
        "<a style=\"float:right\" class=\"btn btn-light\" onclick=\"$(this).parent().next('.toggleable').slideToggle(200)\">Show / Hide</a>",
        "<div class='toggleable'>",
        instrPrepFnc(dctSpec$measure_dev$instruction),
        "</div>",
        "");
  }
  if ("measure_code" %in% include) {
    res <-
      c(res,
        paste0(repStr("#", headingLevel+1), " Instruction for coding measurement instruments"),
        "",
        "<a style=\"float:right\" class=\"btn btn-light\" onclick=\"$(this).parent().next('.toggleable').slideToggle(200)\">Show / Hide</a>",
        "<div class='toggleable'>",
        instrPrepFnc(dctSpec$measure_code$instruction),
        "</div>",
        "");
  }
  if ("manipulate_dev" %in% include) {
    res <-
      c(res,
        paste0(repStr("#", headingLevel+1), " Instruction for developing manipulations"),
        "",
        "<div class='toggleable'>",
        instrPrepFnc(dctSpec$manipulate_dev$instruction),
        "</div>",
        "");
  }
  if ("manipulate_code" %in% include) {
    res <-
      c(res,
        paste0(repStr("#", headingLevel+1), " Instruction for coding manipulations"),
        "",
        "<div class='toggleable'>",
        instrPrepFnc(dctSpec$manipulate_code$instruction),
        "</div>",
        "");
  }
  if ("aspect_dev" %in% include) {
    res <-
      c(res,
        paste0(repStr("#", headingLevel+1), " Instruction for developing aspects"),
        "",
        "<div class='toggleable'>",
        instrPrepFnc(dctSpec$aspect_dev$instruction),
        "</div>",
        "");
  }
  if ("aspect_code" %in% include) {
    res <-
      c(res,
        paste0(repStr("#", headingLevel+1), " Instruction for coding aspects"),
        "",
        "<div class='toggleable'>",
        paste0("*When coding aspects, use the following code: **`dct:", dctSpec$id, "`***"),
        "",
        instrPrepFnc(dctSpec$aspect_code$instruction),
        "</div>",
        "");
  }
  if ("rel" %in% include) {
    res <-
      c(res,
        paste0(repStr("#", headingLevel+1), " Relationships with other constructs"),
        "",
        instrPrepFnc(ifelse(is.null(dctSpec$rel),
                     "*Not specified*",
                     ifelse(all(c("id", "type") %in% names(dctSpec$rel)),
                            paste0("- Related to dct:", dctSpec$rel$id, " with relationship of type ",
                                   dctSpec$rel$type, "\n"),
                            paste0(unlist(lapply(dctSpec$rel, function(i) {
                              return(paste0("- Related to dct:", i$id, " with relationship of type ",
                                            i$type, "\n"));
                            })), collapse="\n")))),
        "");
  }

  return(res);
}
