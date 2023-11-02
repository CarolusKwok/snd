#' @title
#' System tools: Creating appropriate abort messages
#'
#' @description
#' An abort or warning message may involve names of many different objects, all need to be processed and collapsed appropriately for people to understand clearly. These functions below help to collapse different object names into 1 string.
#'
#' @param columns Column names as a vector of characters
#' @param class Classes as a vector of characters
#' @param set Dataset names as a vector of characters
#' @param sheet xlsx sheet names as a vector of characters
#' @param code Code as a vector of characters
#'
#' @return A single, formatted character string, that can be used directly
#' @keywords internal
#' @examples
#' snd:::sys_message_columns(c("a", "b"))
#' snd:::sys_message_class(c("data.frame", "list"))
#' snd:::sys_message_set(c("eco", "water"))
#' snd:::sys_message_sheet(c("#data_eco", "#data_water"))
sys_message_columns = function(columns){
  return(stringr::str_flatten(string = paste0("{.col ", columns, "}"),
                              collapse = ", "))

}

#' @rdname sys_message_columns
#' @keywords internal
sys_message_class = function(class){
  return(stringr::str_flatten(string = paste0("{.cls ", class, "}"),
                              collapse = ", "))
}

#' @rdname sys_message_columns
#' @keywords internal
sys_message_set = function(set){
  return(stringr::str_flatten(string = paste0("{.set ", set, "}"),
                              collapse = ", "))
}

#' @rdname sys_message_columns
#' @keywords internal
sys_message_sheet = function(sheet){
  return(stringr::str_flatten(string = paste0("{.sheet ", sheet, "}"),
                              collapse = ", "))
}


#' @rdname sys_message_columns
#' @keywords internal
sys_message_code = function(code){
  return(stringr::str_flatten(string = paste0("{.code ", code, "}"),
                              collapse = ", "))
}
