base30toNumeric <- function(x) {
  symbols <- rev(strsplit(tolower(x), "")[[1]]);
  res <- 0L;
  base30 <- c(0:9,
              'b', 'c', 'd', 'f', 'g',
              'h' ,'j', 'k', 'l', 'n',
              'p', 'q', 'r', 's', 't',
              'v', 'w', 'x', 'y', 'z');
  for (i in seq_along(symbols)) {
    res <- res + (30^(i-1L) * (which(base30==symbols[i])-1L));
  }
  return(res);
}
