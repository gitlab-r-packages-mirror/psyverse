hyperlink_ucids <- function(x,
                            urlPrefix = "#",
                            regex = "dct:([a-zA-Z0-9_]+)",
                            replacement = paste0("[dct:\\1](",
                                                 urlPrefix,
                                                 "\\1)")) {
  return(gsub(regex,
              replacement,
              x));
}
