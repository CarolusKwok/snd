#' @keywords internal
#' @rdname sys_abort_mtx
sys_abort_mtxColNotAllFilled = function(x, name, columns){
  if(!hasArg(columns)){snd:::sys_abort_NoArg(columns)}
  columns = stringr::str_flatten(paste0("{.col ", columns, "}"), collapse = ", ")
  if(rlang::is_missing(name)){
    snd:::sys_abort(message = c("x" = "Some columns in {.arg {arg}} are not filled",
                                "!" = "Please fill in the following columns:",
                                "!" = columns),
                    arg = rlang::caller_arg(arg = x))
  } else {
    snd:::sys_abort(message = c("x" = "Some columns in {.mtx {name}} are not filled",
                                "!" = "Please correct the following columns:",
                                "!" = columns),
                    arg = rlang::caller_arg(arg = x), name = name)
  }
}
