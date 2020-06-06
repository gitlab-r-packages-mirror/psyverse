dct_object <-
  function(
    version = as.character(packageVersion("psyverse")),
    prefix = paste(sample(letters, 4), collapse=""),
    label = "",
    date = as.character(Sys.Date()),
    ancestry = "",
    retires = "",
    definition = "",
    measure_dev = "",
    measure_code = "",
    aspect_dev = "",
    aspect_code = "",
    rel = NULL
  ) {

  res <-
    list(
      version = version,
      id = psyverse::generate_id(prefix=prefix),
      label = label,
      date = date,
      ancestry = ancestry,
      retires = retires,
      definition = nest_in_list(definition,  nestIn = "definition"),
      measure_dev = nest_in_list(measure_dev,  nestIn = "instruction"),
      measure_code = nest_in_list(measure_code, nestIn = "instruction"),
      aspect_dev = nest_in_list(aspect_dev,   nestIn = "instruction"),
      aspect_code = nest_in_list(aspect_code,  nestIn = "instruction"),
      rel = rel
    );

  class(res) <-
    "psyverse_dct";

  return(res);

}
