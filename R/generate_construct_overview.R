#' Generate construct overviews and instruction overviews
#'
#' These functions use a DCT specification to generate a
#' construct overview or an instruction overview.
#'
#' @param dctSpec The DCT specification, as resulting from a call
#' to [load_dct_specs()] or [load_dct_dir()].
#' @param include Which elements to include in the construct overview.
#' @param hideByDefault Which elements to hide by default.
#' @param divClass The class of the button to collapse/expand sections.
#' @param dctSpecDf The DCT specification dataframer, as produced
#' by a call to [load_dct_specs()] or [load_dct_dir()], and stored within
#' the resulting object.
#' @param type For instruction overviews, the type of instruction
#' to generate can be specified: must be one of "`measure_dev`",
#' "`measure_code`", "`manipulate_dev`", "`manipulate_code`",
#' "`aspect_dev`", or "`aspect_code`".
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
                                        hideByDefault = NULL,
                                        divClass = "btn btn-secondary",
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
                     hideByDefault = hideByDefault,
                     divClass = divClass,
                     headingLevel = headingLevel,
                     hyperlink_ucids = hyperlink_ucids,
                     urlPrefix = urlPrefix
                   )
                 );
               }),
        nm = names(dctSpec$intermediate$dctSpec)));
  }

  defaultDisplay <-
    stats::setNames(rep("block", length(include)),
                    nm = include);

  if (!is.null(hideByDefault)) {
    defaultDisplay[hideByDefault] <-
      "none";
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

  collapseButtonHTML <-
    function(txt) {
      res <-
        sprintf(paste0("<div style=\"float:right\" class=\"",
                       divClass,
                       "\" onclick=\"$(this).next('.toggleable').toggle()\">%s</div>"),
                txt);
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
        "",
        paste0(repStr("#", headingLevel+1), " Definition"),
        collapseButtonHTML(paste0("Show/hide definition")),
        "",
        paste0("<div style='clear: both; display: ", defaultDisplay['definition'], "' class='toggleable'>"),
        instrPrepFnc(dctSpec$definition$definition),
        "</div>",
        "");
  }
  if ("measure_dev" %in% include) {
    res <-
      c(res,
        "",
        paste0(repStr("#", headingLevel+1), " Instruction for developing measurement instruments"),
        collapseButtonHTML(paste0("Show/hide measurement instruction")),
        "",
        paste0("<div style='clear: both; display: ", defaultDisplay['measure_dev'], "' class='toggleable'>"),
        instrPrepFnc(dctSpec$measure_dev$instruction),
        "</div>",
        "");
  }
  if ("measure_code" %in% include) {
    res <-
      c(res,
        "",
        paste0(repStr("#", headingLevel+1), " Instruction for coding measurement instruments"),
        collapseButtonHTML(paste0("Show/hide coding instruction")),
        "",
        paste0("<div style='clear: both; display: ", defaultDisplay['measure_code'], "' class='toggleable'>"),
        instrPrepFnc(dctSpec$measure_code$instruction),
        "</div>",
        "");
  }
  if ("manipulate_dev" %in% include) {
    res <-
      c(res,
        "",
        paste0(repStr("#", headingLevel+1), " Instruction for developing manipulations"),
        collapseButtonHTML,
        "",
        paste0("<div style='clear: both; display: ", defaultDisplay['manipulate_dev'], "' class='toggleable'>"),
        instrPrepFnc(dctSpec$manipulate_dev$instruction),
        "</div>",
        "");
  }
  if ("manipulate_code" %in% include) {
    res <-
      c(res,
        "",
        paste0(repStr("#", headingLevel+1), " Instruction for coding manipulations"),
        collapseButtonHTML,
        "",
        paste0("<div style='clear: both; display: ", defaultDisplay['manipulate_code'], "' class='toggleable'>"),
        instrPrepFnc(dctSpec$manipulate_code$instruction),
        "</div>",
        "");
  }
  if ("aspect_dev" %in% include) {
    res <-
      c(res,
        "",
        paste0(repStr("#", headingLevel+1), " Instruction for developing aspects"),
        collapseButtonHTML,
        "",
        paste0("<div style='clear: both; display: ", defaultDisplay['aspect_dev'], "' class='toggleable'>"),
        instrPrepFnc(dctSpec$aspect_dev$instruction),
        "</div>",
        "");
  }
  if ("aspect_code" %in% include) {
    res <-
      c(res,
        "",
        paste0(repStr("#", headingLevel+1), " Instruction for coding aspects"),
        collapseButtonHTML,
        "",
        paste0("<div style='clear: both; display: ", defaultDisplay['aspect_code'], "' class='toggleable'>"),
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
