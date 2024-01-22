serialize_df <- function(x) {
  return(
    apply(
      X = x,
      MARGIN = 1,
      FUN = function(x) { return(as.list(x)); },
      simplify = FALSE
    )
  );
}
