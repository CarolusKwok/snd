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
sys_abort_mtxDuplicatedColumn = function(x, name){
  colnames = colnames(x)
  dup_columns = unique(colnames[duplicated(colnames)]) %>%
    paste0('{.code ', . , '}') %>%
    stringr::str_flatten(collapse = ", ")

  if(rlang::is_missing(name)){
    snd:::sys_abort(message = c("x" = "Duplicated columns in {.arg {arg}}",
                                    "i" = "Please check columns in {.arg {arg}}",
                                    "i" = "Duplicated Columns:",
                                    "i" = dup_columns),
                        arg = rlang::caller_arg(arg = x))
  } else {
    snd:::sys_abort(message = c("x" = "Duplicated columns in {.mtx {name}}",
                                    "i" = "Please check columns in {.mtx {name}}",
                                    "i" = "Duplicated Columns:",
                                    "i" = dup_columns),
                        arg = rlang::caller_arg(arg = x),
                        name = name)
  }
}
