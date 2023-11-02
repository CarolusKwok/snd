#' @title
#' System tools: Standard abort message for matrices
#'
#' @param x The argument holding the matrix
#' @param name Optional: name of the matrix
#'
#' @return A standard abort message
#' @keywords internal
sys_abort_mtx = function(name){
  snd:::sys_abort(message = c("x" = "Something went wrong in the following matrix",
                              "x" = name))
}

#' @keywords internal
sys_abort_mtxMissingSelectedKey = function(x, keys_missing, name){
  if(rlang::is_missing(x)){snd:::sys_abort_NoArg(x)}
  if(rlang::is_missing(keys_missing)){snd:::sys_abort_NoArg(keys_missing)}
  if(rlang::is_missing(name)){snd:::sys_abort_NoArg(name)}

  snd:::sys_abort(message = c("x" = "Missing keys in {.mtx {name}}",
                              "i" = "Please include keys in {.mtx {name}}",
                              "i" = "Keys missing: {.col {keys_missing}}"),
                  name = name, keys_missing = keys_missing)
}
