#' An easier way to add additional classes to an object
#'
#' @description
#' Self-explanatory... just like the title
#'
#' @param x Any object
#' @param class The class to add. This will always be in front of the original classes of `x`
#'
#' @return `x`, now also classed in `class`
#' @keywords internal
#'
#' @examples classify(X, "snd_data")
classify = function(x, class){
  class(x) = c(class, class(x))
  return(x)
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
