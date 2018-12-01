#' @rdname base30and36conversion
#' @export
base36toNumeric <- function(x) {
  symbols <- rev(strsplit(tolower(x), "")[[1]]);
  res <- 0L;
  base36 <- c(as.character(0:9), letters);
  for (i in seq_along(symbols)) {
    res <- res + (36L^(i-1L) * (which(base36==symbols[i])-1L));
  }
  return(res);
}
