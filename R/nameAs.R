#' An easier way to add names into an object
#'
#' @description
#' Self-explanatory... just like the title
#'
#' @param x Any object
#' @param name names of the item inside of `x`, in character.
#'
#' @return `x`, but with names
#' @keywords internal
nameAs = function(x, name){
  names(x) = name
  return(x)
}
