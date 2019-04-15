generate_instruction_overview <- function(dctSpecDf,
                                          type,
                                          title = paste0("Instructions for", type)) {
  typeInstr <- paste0(type, "_instruction");
  res <-
    c(paste0("# ", title),
      format(Sys.time(), '*Generated at %H:%M:%S on %Y-%m-%d %Z (GMT%z)*'),
      "",
      apply(dctSpecDf[order(dctSpecDf$label), ],
            1,
            function(spec) {
              return(c(paste("##", spec['label']),
                       "",
                       ifelse(is.null(spec[type]),
                              ifelse(is.null(spec[typeInstr]),
                                     "*(Not specified)*",
                                     spec[typeInstr]),
                              spec[type]),
                       ""));
            }));
  return(res);
}
