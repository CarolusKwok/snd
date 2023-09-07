#' @keywords internal
#' @rdname sys_abort_mtx
sys_abort_mtxColWrongClass = function(x, name, columns, expected){
  if(!hasArg(x)){snd:::sys_abort_NoArg(x)}
  if(!hasArg(columns)){snd:::sys_abort_NoArg(columns)}
  if(!hasArg(expected)){snd:::sys_abort_NoArg(expected)}
  columns = stringr::str_flatten(paste0("{.col ", columns, "}"), collapse = ", ")
  expected = stringr::str_flatten(paste0("{.cls ", expected, "}"), collapse = ", ")

  if(rlang::is_missing(name)){
    snd:::sys_abort(message = c("x" = "Wrong class for columns in {.arg {arg}}",
                                "!" = "Columns with wrong class include:",
                                "!" = columns,
                                "i" = "Please use the following classes for those columns:",
                                "i" = expected),
                    arg = rlang::caller_arg(arg = x))
  } else {
    snd:::sys_abort(message = c("x" = "Wrong class for columns in {.mtx {name}}",
                                "!" = "Columns with wrong class include:",
                                "!" = columns,
                                "i" = "Please use the following classes for those columns in {.mtx {name}}:",
                                "i" = expected),
                    arg = rlang::caller_arg(arg = x), name = name)
  }
}
