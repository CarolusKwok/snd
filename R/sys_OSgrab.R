#' Grabs OS content of a particular key
#'
#' @description
#' OSkey refers to the names of items stored in the `snd_os`. This function grabs the content of that key.
#'
#' @param snd snd
#' @param OSkey a character
#'
#' @return value of that key
#' @keywords internal
OSgrab = function(snd, OSkey){
  #Checks ####
  snd:::check_snd(snd)
  if(length(OSkey) != 1){snd:::sys_abort_WrongLength(OSkey, length = 1L)}

  #Magic ####
  OS_index = match(x = TRUE,
                   table = sapply(X = snd,
                                  FUN = function(X){return(snd::is_snd_os(X))}))
  OSkeys = names(snd[[OS_index]])
  OSkey_index = match(x = OSkey, table = OSkeys, nomatch = NA)
  if(is.na(OSkey_index)){
    return(NULL)
  } else {
    return(snd[[OS_index]][[OSkey_index]])
  }
}
