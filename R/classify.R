#' An easier way to add additional classes to an object
#'
#' @description
#' Self-explanatory... just like the title
#' For `classify_key`, the key doesn't need to remove `@` in front. It will automatically remove it for you. However, if you did remove it, it still works
#'
#' @param x Any object
#' @param class The class to add. This will always be in front of the original classes of `x`
#'
#' @return `x`, now also classed in `class`
#' @keywords internal
#'
#' @examples classify(X, "snd_data")
classify = function(x, class){
  if(rlang::is_missing(x)){snd:::sys_abort_NoArg(x)}
  if(rlang::is_missing(class)){snd:::sys_abort_NoArg(class)}

  return(structure(.Data = x, class = c(class, class(x))))
}

#' @keywords internal
#' @rdname classify
classify_factor = function(x){
  return(snd:::classify(x, class = "snd_factor"))
}

#' @keywords internal
#' @rdname classify
classify_item = function(x){
  return(snd:::classify(x, class = "snd_item"))
}


#' @keywords internal
#' @rdname classify
classify_data = function(x){
  return(snd:::classify(x, class = "snd_data"))
}

#' @keywords internal
#' @rdname classify
classify_set = function(x){
  if(rlang::is_missing(x)){snd:::sys_abort_NoArg(x)}
  return(structure(.Data = x, class = "snd_set"))
}

#' @keywords internal
#' @rdname classify
classify_os = function(x){
  if(rlang::is_missing(x)){snd:::sys_abort_NoArg(x)}
  return(structure(.Data = x, class = "snd_os"))
}

#' @keywords internal
#' @rdname classify
classify_snd = function(x){
  if(rlang::is_missing(x)){snd:::sys_abort_NoArg(x)}
  return(structure(.Data = x, class = "snd"))
}

#' @keywords internal
#' @rdname classify
classify_key = function(x){
  if(rlang::is_missing(x)){snd:::sys_abort_NoArg(x)}
  class = paste0("sndkey_", stringr::str_remove(string = x, pattern = "^@"))
  return(structure(.Data = x, class = class))
}
