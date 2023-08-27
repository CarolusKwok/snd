#' @keywords internal
#' @rdname sys_abort_mtx
sys_abort_mtxColUnsupport = function(x, name, columns){
  if(!hasArg(columns)){snd:::sys_abort_NoArg(columns)}
  columns = stringr::str_flatten(paste0("{.col ", columns, "}"), collapse = ", ")
  if(rlang::is_missing(name)){
    snd:::sys_abort(message = c("x" = "Containing unsupported items",
                                "i" = "Please check items in {.arg {arg}}",
                                "i" = "Refer to SND documents or {.code snd:::sys_datatype_available()} for more info",
                                "i" = "Unsupported items:",
                                "i" = columns),
                    arg = rlang::caller_arg(arg = x))
  } else {
    snd:::sys_abort(message = c("x" = "Containing unsupported items",
                                "i" = "Please check items in {.mtx {name}}",
                                "i" = "Refer to SND documents or {.code snd:::sys_datatype_available()} for more info",
                                "i" = "Unsupported items:",
                                "i" = columns),
                    arg = rlang::caller_arg(arg = x), name = name)
  }
}
