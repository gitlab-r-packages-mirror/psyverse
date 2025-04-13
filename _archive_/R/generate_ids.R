#' @rdname generate_id
#' @export
generate_ids <- function(x,
                         stopOnIllegalChars = FALSE) {
  if (is.numeric(x)) {
    x <-
      unlist(
        lapply(
          1:x,
          function(x) return(paste(sample(letters, 4), collapse=""))));
  }
  res <- generate_id(prefix="",
                     stopOnIllegalChars=stopOnIllegalChars);
  if (length(x) == 1) {
    return(res);
  }
  newId <- res;
  for (currentPrefix in x[-1]) {
    while (newId %in% res) {
      newId <- generate_id(prefix="",
                           stopOnIllegalChars=stopOnIllegalChars);
    }
    res <- c(res, newId);
  }
  return(paste0(x, res));
}

# generate_ids(c("attitude_experiential",
#                "attitude_instrumental",
#                "attitude",
#                "intention"));
