nest_in_list <-
  function(
    x,
    nestIn = "instruction"
  ) {

  if (is.list(x) && (length(x) == 1) && (names(x) == nestIn)) {
    return(x);
  } else {
    return(
      stats::setNames(
        list(x),
        nm = nestIn
      )
    );
  }
}
