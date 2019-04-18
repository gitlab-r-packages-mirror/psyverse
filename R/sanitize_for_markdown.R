sanitize_for_markdown <- function(x,
                                  regExReplacements = list(c("\\[", "\\\\["))) {
  return(sanitizer(x, regExReplacements));
}
