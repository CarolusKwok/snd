#' Obtain key execution priority
#'
#' @param key A string, with the class prefixed as "sndkey_"
#'
#' @return A number. The higher the number, the higher the priority, the earlier it will execute.
#' @keywords internal
#' @rdname key_priority
key_priority = function(key){UseMethod(generic = "key_priority", object = key)}

#' @export
#' @rdname key_priority
key_priority.default = function(key){return(0)}

#' @export
#' @rdname key_priority
key_priority.sndkey_type   = function(key){return(10000)}

#' @export
#' @rdname key_priority
key_priority.sndkey_factor = function(key){return( 5000)}

#' @export
#' @rdname key_priority
key_priority.sndkey_item   = function(key){return( 5000)}

#' @export
#' @rdname key_priority
key_priority.sndkey_format = function(key){return( 2500)}

#' @export
#' @rdname key_priority
key_priority.sndkey_label = function(key){return( 1250)}
