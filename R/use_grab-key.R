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
  #Check ####
  if(rlang::is_missing(mtx)){snd:::sys_abort_NoArg(mtx)}
  if(rlang::is_missing(key)){snd:::sys_abort_NoArg(key)}

  #Return all non-standard characters in the beginning of the string ####
  unlist(dplyr::select(.data = mtx, key = {{key}}), use.names = FALSE) %>%
    trimws %>%
    stringr::str_extract(pattern = "^[^A-Za-z0-9]+") %>%
    ifelse(is.na(.), "", .) %>%
    return
}

#' @export
#' @rdname grab_key
grab_keyHead = function(mtx, key){
  #Check ####
  if(rlang::is_missing(mtx)){snd:::sys_abort_NoArg(mtx)}
  if(rlang::is_missing(key)){snd:::sys_abort_NoArg(key)}

  #Return in between Prefix and Tail ####
  process = dplyr::select(.data = mtx, key = {{key}}) %>%
    dplyr::mutate(key = trimws(key)) %>%
    dplyr::mutate(prefix = snd::grab_keyPrefix(mtx = ., key = "key"),
                  tail = snd::grab_keyTail(mtx = ., key = "key"),
                  nchar = nchar(prefix),
                  head = ifelse(nchar == 0, key, stringr::str_sub(string = key, start = (nchar + 1L), end = -1L)),
                  nchar = nchar(tail),
                  head = ifelse(nchar == 0, head, stringr::str_sub(string = head, start = 1L, end = (-nchar - 2L))))
  return(process$head)
}

#' @export
#' @rdname grab_key
grab_keyTail = function(mtx, key){
  #Check ####
  if(rlang::is_missing(mtx)){snd:::sys_abort_NoArg(mtx)}
  if(rlang::is_missing(key)){snd:::sys_abort_NoArg(key)}

  #Return all characters behind the first _ ####
  unlist(dplyr::select(.data = mtx, key = {{key}}), use.names = FALSE) %>%
    trimws %>%
    stringr::str_extract(pattern = "(?<=_).*") %>%
    ifelse(is.na(.), "", .) %>%
    return
}
