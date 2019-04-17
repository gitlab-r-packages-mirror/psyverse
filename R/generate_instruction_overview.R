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
              return(c(paste("##", spec['label']),
                       paste0("When coding aspects, use the following code (including square brackets): **`[dct:", spec['id'], "]`**"),
                       "",
                       ifelse(is.null(spec[typeInstr]),
                              "*(Not specified)*",
                              spec[typeInstr]),
                       ""));
            }));
  return(res);
}
