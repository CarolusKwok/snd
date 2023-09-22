#' Detect what format a vector should be.
#' The detection is based on how many NAs there are. The more NAs, the more unlikely the format supports it.
#'
#' @param column A column of items in data.frame
#'
#' @return a character of the expected formats
#' @keywords internal
#'
#' @examples format_detect(column)
format_detect = function(column){
  supportedFormats = snd:::sys_format_support(with_abbr = FALSE)
  format = lapply(X = supportedFormats,
                  FUN = function(X){return(snd:::classify(X, class = paste0("DT", X)))})

  formated_data = lapply(X = format,
                         FUN = snd:::formatRI_key2mtx_format_use,
                         colItem = column,
                         force = TRUE) %>%
    snd:::nameAs(supportedFormats) %>%
    as.data.frame() %>%
    dplyr::mutate_all(is.na) %>%
    colSums()
  formated = unlist(formated_data) %>% .[. == min(.)]
  return(invisible(names(formated[1])))
}
