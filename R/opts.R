#' Options for the psyverse package
#'
#' The `psyverse::opts` object contains three functions to set, get, and reset
#' options used by the escalc package. Use `psyverse::opts$set` to set options,
#' `psyverse::opts$get` to get options, or `psyverse::opts$reset` to reset specific or
#' all options to their default values.
#'
#' It is normally not necessary to get or set `psyverse` options.
#'
#' The following arguments can be passed:
#'
#' \describe{
#'   \item{...}{For `psyverse::opts$set`, the dots can be used to specify the options
#'   to set, in the format `option = value`, for example,
#'   `encoding = "UTF-8"`. For
#'   `psyverse::opts$reset`, a list of options to be reset can be passed.}
#'   \item{option}{For `psyverse::opts$set`, the name of the option to set.}
#'   \item{default}{For `psyverse::opts$get`, the default value to return if the
#'   option has not been manually specified.}
#' }
#'
#' The following options can be set:
#'
#' \describe{
#'
#'   \item{encoding}{The default encoding used to read or write files.}
#'
#' }
#'
#' @aliases opts set get reset
#'
#' @usage opts
#'
#' @examples ### Get the default encoding
#' psyverse::opts$get(encoding);
#'
#' ### Set it to UTF-8-BOM
#' psyverse::opts$set(encoding = "UTF-8-BOM");
#'
#' ### Check that it worked
#' psyverse::opts$get(encoding);
#'
#' ### Reset this option to its default value
#' psyverse::opts$reset(encoding);
#'
#' ### Check that the reset worked, too
#' psyverse::opts$get(encoding);
#'
#' @export
opts <- list();

opts$set <- function(...) {
  dots <- list(...);
  dotNames <- names(dots);
  names(dots) <-
    paste0("psyverse.", dotNames);
  if (all(dotNames %in% names(opts$defaults))) {
    do.call(options,
            dots);
  } else {
    stop("Option '", option, "' is not a valid (i.e. existing) option for psyverse!");
  }
}

opts$get <- function(option, default=FALSE) {
  option <- as.character(substitute(option));
  if (!option %in% names(opts$defaults)) {
    stop("Option '", option, "' is not a valid (i.e. existing) option for psyverse!");
  } else {
    return(getOption(paste0("psyverse.", option),
                     opts$defaults[[option]]));
  }
}

opts$reset <- function(...) {
  optionNames <-
    unlist(lapply(as.list(substitute(...())),
                  as.character));
  if (length(optionNames) == 0) {
    do.call(opts$set,
            opts$defaults);
  } else {
    prefixedOptionNames <-
      paste0("psyverse.", optionNames);
    if (all(optionNames %in% names(opts$defaults))) {
      do.call(opts$set,
              opts$defaults[optionNames]);
    } else {
      invalidOptions <-
        !(optionNames %in% names(opts$defaults));
      stop("Option(s) ", vecTxtQ(optionNames[invalidOptions]),
           "' is/are not a valid (i.e. existing) option for psyverse!");
    }
  }
}

opts$defaults <-
  list(
    ### Encoding for files
    encoding = "UTF-8",
    preventOverwriting = TRUE,

    ### Column names for DCT spreadsheets
    dct_sheet_fieldCol = "field",
    dct_sheet_contentCol = "content",

    ### Throttling for google spreasheets
    throttleSeconds = 10,

  )

