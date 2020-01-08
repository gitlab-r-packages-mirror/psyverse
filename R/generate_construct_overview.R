generate_construct_overview <- function(dctSpec,
                                        headingLevel = 3,
                                        hyperlink_ucids = "Markdown") {

  instrPrepFnc <- function(x) {
    if (is.null(x)) {
      res <- "*Not specified*";
    } else {
      res <-
        gsub("\\n", "\n\n", x);
      ### Replace links to DCTs with hyperlinks
      if (hyperlink_ucids == "Markdown") {
        res <- hyperlink_ucids(res);
      } else if (hyperlink_ucids == "HTML") {
        res <- hyperlink_ucids(res,
                               replacement = '<a href="#\\1">dct:\\1</a>');
      }
    }
    return(res);
  }

  res <-
    c("",
      paste0(ufs::repStr("#", headingLevel), " ", dctSpec$label, " {#", dctSpec$id, "}"),
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
      "",
      paste0(ufs::repStr("#", headingLevel+1), " Definition"),
      "",
      instrPrepFnc(dctSpec$definition$definition),
      "",
      paste0(ufs::repStr("#", headingLevel+1), " Instruction for developing measurement instruments"),
      "",
      instrPrepFnc(dctSpec$measure_dev$instruction),
      "",
      paste0(ufs::repStr("#", headingLevel+1), " Instruction for coding measurement instruments"),
      "",
      instrPrepFnc(dctSpec$measure_code$instruction),
      "",
      paste0(ufs::repStr("#", headingLevel+1), " Instruction for developing manipulations"),
      "",
      instrPrepFnc(dctSpec$manipulate_dev$instruction),
      "",
      paste0(ufs::repStr("#", headingLevel+1), " Instruction for coding manipulations"),
      "",
      instrPrepFnc(dctSpec$manipulate_code$instruction),
      "",
      paste0(ufs::repStr("#", headingLevel+1), " Instruction for developing aspects"),
      "",
      instrPrepFnc(dctSpec$aspect_dev$instruction),
      "",
      paste0(ufs::repStr("#", headingLevel+1), " Instruction for coding aspects"),
      "",
      paste0("*When coding aspects, use the following code: **`dct:", dctSpec$id, "`***"),
      "",
      instrPrepFnc(dctSpec$aspect_code$instruction),
      "",
      paste0(ufs::repStr("#", headingLevel+1), " Relationships with other constructs"),
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

  return(res);
}
