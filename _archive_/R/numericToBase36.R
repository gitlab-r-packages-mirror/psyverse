### Function based on the one from Joshua Ulrich
### https://stackoverflow.com/questions/36036404/convert-an-integer-to-base36

#' @rdname base30and36conversion
#' @export
numericToBase36 <- function(x) {
  base36 <- as.character(0:9, letters);
  result <- character();
  i <- 1L;
  while (x > 0) {
    result[i] <- base36[x %% 36L + 1L];
    i <- i + 1L;
    x <- x %/% 36L;
  }
  res <- paste(rev(result), sep="", collapse="")
  return(res);
}
