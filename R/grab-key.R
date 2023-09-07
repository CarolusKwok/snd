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
  alphabet = stringr::str_split_1(string = intToUtf8(c(65:90, 97:122)), pattern = "") %>%
    stringr::str_flatten(collapse = "|")
  mtx = dplyr::select(.data = mtx, key = {{key}}) %>%
    dplyr::mutate(pos = stringr::str_locate(string = key, pattern = alphabet)[,"start"]-1,
                  prefix = ifelse(pos == 0,
                                  NA,
                                  stringr::str_sub(string = key, start = 1L, end = pos)))
  return(mtx$prefix)
}

#' @export
#' @rdname grab_key
grab_keyHead = function(mtx, key){
  alphabet = stringr::str_split_1(string = intToUtf8(c(65:90, 97:122)), pattern = "") %>%
    stringr::str_flatten(collapse = "|")
  mtx = dplyr::select(.data = mtx, key = {{key}}) %>%
    dplyr::mutate(pos_start = stringr::str_locate(string = key, pattern = alphabet)[,"start"],
                  pos_end = stringr::str_locate(string = key, pattern = "_")[,"start"],
                  pos_end = ifelse(is.na(pos_end), -1L, (pos_end-1)),
                  head= stringr::str_sub(string = key, start = pos_start, end = pos_end))
  return(mtx$head)
}

#' @export
#' @rdname grab_key
grab_keyTail = function(mtx, key){
  mtx = dplyr::select(.data = mtx, key = {{key}}) %>%
    dplyr::mutate(pos = stringr::str_locate(string = key, pattern = "_")[,"start"],
                  consist_tail = !is.na(pos),
                  tail = ifelse(consist_tail,
                                stringr::str_sub(string = key, start = pos + 1, end = -1L),
                                NA))
  return(mtx$tail)
}
