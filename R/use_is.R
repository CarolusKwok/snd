#' Test if an object is an object from `snd`
#'
#' Returns `TRUE` for said item being a snd_xxx, and `FALSE` for all other objects, including lists.
#'
#' @param x An object
#'
#' @return A logical value. `TRUE` for SED data.
#' @export
is_snd = function(x){inherits(x, "snd")}

#' @export
is_snd_set = function(x){inherits(x, "snd_set")}

#' @export
is_snd_factor = function(x){inherits(x, "snd_factor")}

#' @export
is_snd_item = function(x){inherits(x, "snd_item")}

#' @export
is_snd_data = function(x){inherits(x, "snd_data")}

#' @export
is_snd_os = function(x){inherits(x, "snd_os")}
