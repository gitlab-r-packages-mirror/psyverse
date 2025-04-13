sanitize_for_DiagrammeR <- function(x,
                                    regExReplacements = list(c("\\\"", "`"),
                                                             c("\\'", "`"),
                                                             c("\\\\", "/"))) {
  return(sanitizer(x, regExReplacements));
}
