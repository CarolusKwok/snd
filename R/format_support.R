#' List out all supported formats in SND
#'
#' @return A list of supported formats/ classes, in `character`
#'
#' @keywords internal
#' @examples sys_format_support()
sys_format_support = function(with_abbr = F){
  data = tibble::tibble(type = c("full",     "alias",   "abbr", "default"),
                        `1`  = c("POSIXct",  "time",       "t",        NA),
                        `2`  = c("integer",  "int",        "i",       "0"),
                        `3`  = c("numeric",  "num",        "n",       "0"),
                        `4`  = c("logical",  "boolean",    "l",       "n"),
                        `5`  = c("character","string",     "c",        NA)) %>%
    tibble::column_to_rownames(var = "type") %>%
    base::t() %>%
    tibble::as_tibble()
  if(!with_abbr){
    return(data$full)
  } else {
    return(data)
  }
}
