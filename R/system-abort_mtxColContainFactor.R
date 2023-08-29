#' @keywords internal
#' @rdname sys_abort_mtx
sys_abort_mtxColContainFactor = function(x, name, columns){
  if(!hasArg(x)){snd:::sys_abort_NoArg(x)}
  if(!hasArg(columns)){snd:::sys_abort_NoArg(columns)}

  columns = stringr::str_flatten(paste0("{.col ", columns, "}"), collapse = ", ")
  if(rlang::is_missing(name)){
    snd:::sys_abort(message = c("x" = "{.arg {arg}} contain factors in columns",
                                "!" = "Please correct the following columns:",
                                "!" = columns),
                    arg = rlang::caller_arg(arg = x))
  } else {
    snd:::sys_abort(message = c("x" = "{.mtx {name}} contain factors in columns",
                                "!" = "Please correct the following columns:",
                                "!" = columns),
                    arg = rlang::caller_arg(arg = x), name = name)
  }
}
