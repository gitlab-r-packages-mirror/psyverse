numericToBase30 <- function(x) {
  base30 <- c(0:9,
              'b', 'c', 'd', 'f', 'g',
              'h' ,'j', 'k', 'l', 'n',
              'p', 'q', 'r', 's', 't',
              'v', 'w', 'x', 'y', 'z');
  result <- character();
  i <- 1L;
  while (x > 0) {
    result[i] <- base30[x %% 30L + 1L];
    i <- i + 1L;
    x <- x %/% 30L;
  }
  res <- paste(rev(result), sep="", collapse="")
  return(res);
}
