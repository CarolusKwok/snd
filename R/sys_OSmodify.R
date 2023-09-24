#' Modifies OS content
#'
#' @description
#' OSkey refers to the names of items stored in the `snd_os`. Modifing this `snd_os` will lead to behavior change in `snd` and `seal`.
#'
#'
#' @param snd snd
#' @param OSkey a character
#' @param value the value of the `OSkey`
#'
#' @return `snd`, with OS modified
#' @keywords internal
OSmodify = function(snd, OSkey, value){
  #Checks ####
  snd:::check_snd(snd)
  if(length(OSkey) != 1){snd:::sys_abort_WrongLength(OSkey, length = 1L)}

  #Magic ####
  OS_index = match(x = TRUE,
                   table = sapply(X = snd,
                                  FUN = function(X){return(snd::is_snd_os(X))}))
  OSkeys = names(snd[[OS_index]])
  OSkey_index = match(x = OSkey, table = OSkeys, nomatch = length(OSkeys) + 1)

  OSkeys[[OSkey_index]] = OSkey
  snd[[OS_index]][[OSkey_index]] = value
  names(snd[[OS_index]]) = OSkeys
  return(snd)
}
