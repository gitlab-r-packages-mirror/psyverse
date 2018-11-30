extract_dct_specs <- function(file,
                              delimiterRegEx = "^---$",
                              ignoreOddDelimiters = FALSE,
                              silent=FALSE) {

    yamlLineSets <- extract_yaml_fragments(file,
                                           delimiterRegEx=delimiterRegEx,
                                           ignoreOddDelimiters=ignoreOddDelimiters,
                                           silent=TRUE);

    pastedYamlLineSets <- lapply(yamlLineSets,
                                 paste,
                                 collapse="\n");

    rawSpecs <- lapply(yamlLineSets,
                       yaml::yaml.load);

    rawSpecs <-
      lapply(rawSpecs,
             function(x) {
               if (is.null(x$date)) {
                 return(x)
               } else if (nchar(trimws(x$date))==10) {
                 x$datetime <- as.numeric(strptime(x$date, "%Y-%m-%d"));
               } else if (nchar(trimws(x$date))==16) {
                 x$datetime <- as.numeric(strptime(x$date, "%Y-%m-%d %H:%M"));
               }
               return(x);
             });

    rawSpecs <- lapply(rawSpecs,
                       structure,
                       class='dtcRawSpec');

    return(structure(rawSpecs,
                     class="dtcRawSpecList"));

  }

#x <- extract_dct_specs("B:/Data/R/tmp/attitude_experiential.dct");

