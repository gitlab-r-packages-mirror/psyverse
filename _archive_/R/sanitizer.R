sanitizer <- function(x, regExReplacements) {
  regExReplacements <-
    regExReplacements[unlist(lapply(regExReplacements,
                                    function(elements) {
                                      return((length(elements)==2) && is.character(elements));
                                    }))];
  for (i in seq_along(regExReplacements)) {
    x <- gsub(regExReplacements[[i]][1],
              regExReplacements[[i]][2],
              x,
              perl=TRUE);
  }
  return(x);
}
