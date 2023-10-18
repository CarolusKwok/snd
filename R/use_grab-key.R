#' @title
#' System tools: Grab properties of key
#'
#' @description
#' Properties of a key is specified and separated using "_". These functions find the "head" and "tail" part of the key.
#'
#' The functions are as follow:
#' grab_keyHead: Obtain the head of the key
#' grab_keyTail: Obtain the tail of the key
#'
#' @param mtx mtx itself
#' @param key the name of the key. Only accepts characters and only accept 1
#'
#' @return A string of characters
#' @export
#' @rdname grab_key
grab_keyPrefix = function(mtx, key){
  if(rlang::is_missing(mtx)){snd:::sys_abort_NoArg(mtx)}
  if(rlang::is_missing(key)){snd:::sys_abort_NoArg(key)}

  dplyr::select(.data = mtx, key = {{key}}) %>%
    unlist() %>%
    stringr::str_extract(pattern = "(.*_)|(.*)") %>%
    stringr::str_remove(pattern = "_$") %>%
    stringr::str_remove(pattern = "[A-Za-z0-9]+$") %>%
    return()
}

#' @export
#' @rdname grab_key
grab_keyHead = function(mtx, key){
  if(rlang::is_missing(mtx)){snd:::sys_abort_NoArg(mtx)}
  if(rlang::is_missing(key)){snd:::sys_abort_NoArg(key)}

  dplyr::select(.data = mtx, key = {{key}}) %>% unlist() %>%
    stringr::str_extract(pattern = "(.*_)|(.*)") %>%
    stringr::str_extract(pattern = "[A-Za-z0-9]+") %>%
    return()
}

#' @export
#' @rdname grab_key
grab_keyTail = function(mtx, key){
  if(rlang::is_missing(mtx)){snd:::sys_abort_NoArg(mtx)}
  if(rlang::is_missing(key)){snd:::sys_abort_NoArg(key)}

  dplyr::select(.data = mtx, key = {{key}}) %>% unlist %>%
    stringr::str_extract(pattern = "_.*") %>%
    stringr::str_remove(pattern = "_") %>%
    return()
}
