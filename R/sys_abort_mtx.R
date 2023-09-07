#' @title
#' System tools: Standard abort message for matrices
#'
#' @param x The argument holding the matrix
#' @param name Optional: name of the matrix
#'
#' @return A standard abort message
#' @keywords internal
#'
#' @rdname sys_abort_mtx
sys_abort_mtx = function(name){
  snd:::sys_abort(message = c("x" = "Something went wrong in the following matrix",
                              "x" = name))
}
