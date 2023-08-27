#' Test if an object is an object from `snd`
#'
#' Returns `TRUE` for SED data, and `FALSE` for all other objects, including lists.
#'
#' @param x An object
#'
#' @return A logical value. `TRUE` for SED data.
#' @export
#'
#' @examples is_snd(x)
#' @rdname is_snd
is_snd = function(x){inherits(x, "snd")}

#' @export
#' @rdname is_snd
is_snd_set = function(x){inherits(x, "snd_set")}

#' @export
#' @rdname is_snd
is_snd_factor = function(x){inherits(x, "snd_factor")}

#' @export
#' @rdname is_snd
is_snd_item = function(x){inherits(x, "snd_item")}

#' @export
#' @rdname is_snd
is_snd_data = function(x){inherits(x, "snd_data")}
