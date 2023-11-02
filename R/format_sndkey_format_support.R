#' List out all supported formats in SND
#'
#' @param with_abbr A logical value. If `TRUE`, returns with the alias, abbreviations, and default values for each type of data. By default (`FALSE`), returns none of the above.
#'
#' @return A list of supported formats/ classes, in `character`
#'
#' @keywords internal
#' @examples snd:::sys_format_support()
sys_format_support = function(with_abbr = FALSE){
  #1. Check with_abbr####
  if(rlang::is_missing(with_abbr)){snd:::sys_abort_NoArg(with_abbr)}
  if(!is.logical(with_abbr)){snd:::sys_abort_WrongClass(with_abbr, class = "logical")}

  #2. Returns with/ without abbr ####
  data = data.frame(full   = c("POSIXct", "integer", "numeric", "logical", "character"),
                    alias  = c(   "time",     "int",     "num", "boolean",    "string"),
                    abbr   = c(      "t",       "i",       "n",       "l",         "c"),
                    default= c(       NA,       "0",       "0",       "n",          NA))
  if(with_abbr){
    return(data)
  } else {
    return(data$full)
  }
}
