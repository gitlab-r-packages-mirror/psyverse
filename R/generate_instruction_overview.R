#' @rdname overview_generation
#' @export
generate_instruction_overview <- function(dctSpecDf,
                                          type,
                                          headingLevel = 3,
                                          hyperlink_ucids = "Markdown",
                                          urlPrefix = "#") {
  typeInstr <- paste0(type, "_instruction");

  ### Switch order of type and activity
  title <-
    gsub("([^_]*)_([^_]*)",
         "\\2 \\1",
         type);

  ### Writing words in full
  title <- gsub("dev", "developing", title);
  title <- gsub("code", "coding", title);
  title <- gsub("measure", "measurement instruments", title);
  title <- gsub("manipulate", "manipulations", title);
  title <- gsub("aspect", "aspects", title);

  res <-
    c(paste0(repStr("#", headingLevel), " Instruction for ", title),
      format(Sys.time(), '*This overview was generated on %Y-%m-%d at %H:%M:%S %Z (GMT%z)*'),
      "",
      apply(dctSpecDf[order(dctSpecDf$label), ],
            1,
            function(spec) {
              if (type == "aspect_code") {
                extraInfo <-
                  c(paste0("*When coding aspects, use the following code: **`dct:", spec['dct_id'], "`***"),
                    "");
              } else {
                extraInfo <-
                  "";
              }
              res <-
                ifelse(is.null(spec[typeInstr]) || is.na(spec[typeInstr]) || (nchar(spec[typeInstr])==0),
                       "*(Not specified)*",
                       spec[typeInstr]);
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
