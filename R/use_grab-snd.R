#' System tools: Grab various snd objects from a list
#'
#' @param snd
#'
#' @return A list of objects
#' @export
#'
#' @rdname grab_snd
grab_sndset = function(snd){
  return(snd[sapply(snd, FUN = snd::is_snd_set, simplify = TRUE, USE.NAMES = FALSE)])
}

#' @export
#' @rdname grab_snd
grab_sndfactor = function(snd){
  return(snd[sapply(snd, FUN = snd::is_snd_factor, simplify = TRUE, USE.NAMES = FALSE)])
}

#' @export
#' @rdname grab_snd
grab_snditem = function(snd){
  return(snd[sapply(snd, FUN = snd::is_snd_item, simplify = TRUE, USE.NAMES = FALSE)])
}

#' @export
#' @rdname grab_snd
grab_snddata = function(snd){
  return(snd[sapply(snd, FUN = snd::is_snd_data, simplify = TRUE, USE.NAMES = FALSE)])
}

#' @export
#' @rdname grab_snd
grab_sndos = function(snd){
  return(snd[sapply(snd, FUN = snd::is_snd_os, simplify = TRUE, USE.NAMES = FALSE)])
}
