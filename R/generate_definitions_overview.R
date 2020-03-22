#' @rdname overview_generation
#' @export
generate_definitions_overview <- function(dctSpecDf,
                                          headingLevel = 3,
                                          hyperlink_ucids = "Markdown",
                                          urlPrefix = "#") {

  res <-
    c(paste0(repStr("#", headingLevel), " Construct definitions"),
      format(Sys.time(), '*This overview was generated on %Y-%m-%d at %H:%M:%S %Z (GMT%z)*'),
      "",
      apply(dctSpecDf[order(dctSpecDf$label), ],
            1,
            function(spec) {
              extraInfo <-
                c(paste0("Unique Construct Identifier (UCID): `", spec['dct_id'], "`"),
                  "");
              res <-
                ifelse(is.null(spec['definition']) || is.na(spec['definition']) || (nchar(spec['definition'])==0),
                       "*(Not specified)*",
                       spec['definition']);
              titleBit <- paste(repStr("#", headingLevel+1), " ", spec['label']);

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

              return(c(titleBit,
                       extraInfo,
                       res,
                       ""));
            }));

  return(res);
}
