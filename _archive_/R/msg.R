msg <- function(...,
                silent = psyverse::opts$get("silent")) {
  if (!silent) {
    cat0(...);
  }
  return(
    invisible(
      paste0(
        ...
      )
    )
  );
}
