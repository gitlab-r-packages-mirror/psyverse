generate_instruction_overview <- function(dctSpecDf,
                                          type,
                                          title = paste("Instructions for", type)) {
  typeInstr <- paste0(type, "_instruction");

  res <-
    c(paste0("# ", title),
      format(Sys.time(), '*Generated at %H:%M:%S on %Y-%m-%d %Z (GMT%z)*'),
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
              return(c(paste("##", spec['label']),
                       extraInfo,
                       ifelse(is.null(spec[typeInstr]) || is.na(spec[typeInstr]) || (nchar(spec[typeInstr])==0),
                              "*(Not specified)*",
                              spec[typeInstr]),
                       ""));
            }));
  return(res);
}
