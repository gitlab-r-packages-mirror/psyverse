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
#' @param HTMLoutput Whether to output to Markdown (`FALSE`) or HTML (`TRUE`).
#' @param collapseButtons Whether to include buttons to show/hide the definition
#' and instructions.
#' @param hyperlink_UCIDs Whether to create hyperlinks to UCIDs.
#' @param urlPrefix The prefix to insert before the URL in the produced
#' hyperlink. The default, "`#`", results in a link to an
#' anchor (an HTML `a` element) on the current page.
#' @param sortDecreasing Whether to sort the constructs in decreasing order
#' (`TRUE`), in increasing order (`FALSE`), or not at all (`NULL`).
#' @return A character string with the overview.
#' @rdname overview_generation
#' @aliases generate_construct_overview
#' generate_instruction_overview
#' generate_definitions_overview
#' @export
#'
#' @examples exampleDCT <-
#'   psyverse::dct_object(
#'     prefix = "exampleConstruct",
#'     label = "An example construct",
#'     definition = "The definition goes here",
#'     measure_dev = "Here you can explain how to measure the construct"
#'   );
#' generate_construct_overview(exampleDCT);
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
                                        collapseButtons = TRUE,
                                        hyperlink_UCIDs = TRUE,
                                        HTMLoutput = FALSE,
                                        urlPrefix = "#",
                                        sortDecreasing = FALSE) {

  if (inherits(dctSpec, "dct_specs")) {
    return(

      ### Check sortDecreasing and maybe sort

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
                     hyperlink_UCIDs = hyperlink_UCIDs,
                     HTMLoutput = HTMLoutput,
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

  javaScriptBit <-
    "<script>function toggleVisibility(elementId) {
  var x = document.getElementById(elementId);
  if (x.style.display === \"none\") {
    x.style.display = \"block\";
  } else {
    x.style.display = \"none\";
  }
}</script>";

  instrPrepFnc <- function(x) {
    if (is.null(x)) {
      if (HTMLoutput) {
        res <- "<em>Not specified</em>";
      } else {
        res <- "*Not specified*";
      }
    } else {
      if (HTMLoutput) {
        res <-
          gsub("\\n", "<br /><br />", x);
      } else {
        res <-
          gsub("\\n", "\n\n", x);
      }
      ### Replace links to DCTs with hyperlinks
      if (hyperlink_UCIDs) {
        if (HTMLoutput) {
          res <- hyperlink_ucids(res,
                                 replacement = paste0('<a href="',
                                                      urlPrefix,
                                                      '\\1">dct:\\1</a>'));
        } else {
          res <- hyperlink_ucids(res,
                                 urlPrefix = urlPrefix);
        }
      }
    }
    return(res);
  }

  if (collapseButtons) {
    collapseButtonHTML <- function(txt, id) {
        res <-
          sprintf(paste0("<div style=\"float:right;clear:both;",
                         "margin:2px;",
                         "border:2px solid #87C3FF;",
                         "background-color:#c7e3ff;\" class=\"",
                         divClass,
                         "\" onclick=\"toggleVisibility('", id, "');\">%s</div>"),
                  txt);
        return(res);
      }
  } else {
    collapseButtonHTML <- function(txt, id) {
      return("");
    }
  }

  timeBit <-
    format(Sys.time(), 'This overview was generated on %Y-%m-%d at %H:%M:%S %Z (GMT%z)');

  if (HTMLoutput) {
    headingBit <-
      paste0("<a id=", dctSpec$id,"><h", headingLevel, ">",
             dctSpec$label, "</a></h", headingLevel, ">");
    subHeadingFunc <- function(x) {
      return(paste0("<h", headingLevel+1, ">", x,
                    "</h", headingLevel+1, ">"));
    }
    emFunc <- function(x) {
      return(paste0("<em>", x, "</em>"));
    }
    timeBit <- paste0("<div><em>", timeBit, "</em></div>");
    dateBit <-
      paste0("<div>This Decentralized Construct Taxonomy specification was authored at ",
             ifelse(is.null(dctSpec$date),
                    "an unknown date (i.e. this was not specified in the DCT specification)",
                    dctSpec$date),
             ".</div>");
    ucidBit <- paste0("<div>Unique Construct Identifier (UCID): <pre style=\"display:inline;\">", dctSpec$id,
                      "</pre></div>");
    codingInstrBit <-
      paste0(
        "<p<",
        emFunc(
          paste0(
            "When coding aspects, use the following code: ",
            "<strong><pre style=\"display:inline;\">", dctSpec$id, "</pre></strong>"
          )
        ),
        "</p>"
      );
  } else {

    headingBit <-
      paste0(repStr("#", headingLevel), " ", dctSpec$label,
             " {#", dctSpec$id, "}");
    subHeadingFunc <- function(x) {
      return(paste0(repStr("#", headingLevel+1), " ", x));
    }
    emFunc <- function(x) {
      return(paste0("*", x, "*"));
    }
    timeBit <- emFunc(timeBit);
    dateBit <-
      paste0("This Decentralized Construct Taxonomy specification was authored at ",
             ifelse(is.null(dctSpec$date),
                    "an unknown date (i.e. this was not specified in the DCT specification)",
                    dctSpec$date),
             ".");
    ucidBit <- paste0("Unique Construct Identifier (UCID): `", dctSpec$id,
                      "`");
    codingInstrBit <-
      emFunc(
        paste0(
          "When coding aspects, use the following code: **`dct:", dctSpec$id, "`**\n\n"
        )
      );

  }

  res <-
    c("",
      javaScriptBit,
      "",
      headingBit,
      "",
      timeBit,
      "",
      dateBit,
      "",
      ucidBit,
      "");

  defId <- paste0(dctSpec$id, "_definition");
  measureDevId <- paste0(dctSpec$id, "_measure_dev");
  measureCodeId <- paste0(dctSpec$id, "_measure_code");
  aspectDevId <- paste0(dctSpec$id, "_aspect_dev");
  aspectCodeId <- paste0(dctSpec$id, "_measure_code");

  if ("definition" %in% include) {
    res <-
      c(res,
        "",
        subHeadingFunc("Definition"),
        collapseButtonHTML("Show/hide definition",
                           id=defId),
        "",
        paste0("<div id=\"", defId, "\", style='clear: both; display: ",
               defaultDisplay['definition'], "' class='toggleable'>"),
        instrPrepFnc(dctSpec$definition$definition),
        "</div>",
        "<div style='clear:both;height:2px;'></div>",
        "");
  }
  if ("measure_dev" %in% include) {
    res <-
      c(res,
        "",
        subHeadingFunc("Instruction for developing measurement instruments"),
        collapseButtonHTML("Show/hide measurement instruction",
                           id = measureDevId),
        "",
        paste0("<div id=\"",
               measureDevId,
               "\" style='clear: both; display: ", defaultDisplay['measure_dev'], "' class='toggleable'>"),
        instrPrepFnc(dctSpec$measure_dev$instruction),
        "</div>",
        "<div style='clear:both;height:2px;'></div>",
        "");
  }
  if ("measure_code" %in% include) {
    res <-
      c(res,
        "",
        subHeadingFunc("Instruction for coding measurement instruments"),
        collapseButtonHTML("Show/hide measurement coding instruction",
                           id = measureCodeId),
        "",
        paste0("<div id=\"",
               measureCodeId,
               "\" style='clear: both; display: ", defaultDisplay['measure_code'], "' class='toggleable'>"),
        instrPrepFnc(dctSpec$measure_code$instruction),
        "</div>",
        "<div style='clear:both;height:2px;'></div>",
        "");
  }
  if ("aspect_dev" %in% include) {
    res <-
      c(res,
        "",
        subHeadingFunc("Instruction for developing aspects"),
        collapseButtonHTML("Show/hide aspect elicitation instruction",
                           id = aspectDevId),
        "",
        paste0("<div id=\"",
               aspectDevId,
               "\" style='clear: both; display: ", defaultDisplay['aspect_dev'], "' class='toggleable'>"),
        instrPrepFnc(dctSpec$aspect_dev$instruction),
        "</div>",
        "<div style='clear:both;height:2px;'></div>",
        "");
  }
  if ("aspect_code" %in% include) {
    res <-
      c(res,
        "",
        subHeadingFunc("Instruction for coding aspects"),
        collapseButtonHTML("Show/hide aspect coding instruction",
                           id = aspectCodeId),
        "",
        paste0("<div id=\"",
               aspectCodeId,
               "\" style='clear: both; display: ", defaultDisplay['aspect_code'], "' class='toggleable'>"),
        codingInstrBit,
        "",
        instrPrepFnc(dctSpec$aspect_code$instruction),
        "</div>",
        "<div style='clear:both;height:2px;'></div>",
        "");
  }
  if ("rel" %in% include) {
    res <-
      c(res,
        subHeadingFunc("Relationships with other constructs"),
        "",
        instrPrepFnc(ifelse(is.null(dctSpec$rel),
                            emFunc("Not specified"),
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
