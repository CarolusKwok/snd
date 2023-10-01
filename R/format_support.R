#' List out all supported formats in SND
#'
#' @param with_abbr A logical value. If `TRUE`, returns with the alias, abbreviations, and default values for each type of data.
#'
#' @return A list of supported formats/ classes, in `character`
#'
#' @keywords internal
#' @examples sys_format_support()
sys_format_support = function(with_abbr = F){
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
