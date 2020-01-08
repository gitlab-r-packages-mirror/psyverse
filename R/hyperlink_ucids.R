hyperlink_ucids <- function(x,
                            regex = "dct:([a-zA-Z0-9_]+)",
                            replacement = "(dct:\\1)[#\\1]") {
  return(gsub(regex,
              replacement,
              x));
}
