#' @keywords internal
#' @rdname sys_abort_mtx
sys_abort_mtxColNotAllFilledEmptied = function(x, name, columns){
  if(!hasArg(columns)){snd:::sys_abort_NoArg(columns)}
  columns = stringr::str_flatten(paste0("{.col ", columns, "}"), collapse = ", ")
  if(rlang::is_missing(name)){
    snd:::sys_abort(message = c("x" = "Semi-filled/ emptied columns in {.arg {arg}}",
                                "!" = "Please check in the following column:",
                                "!" = columns),
                    arg = rlang::caller_arg(arg = x))
  } else {
    snd:::sys_abort(message = c("x" = "Semi-filled/ emptied columns in {.mtx {name}}",
                                "!" = "Please check in the following column:",
                                "!" = columns),
                    arg = rlang::caller_arg(arg = x), name = name)
  }
}
