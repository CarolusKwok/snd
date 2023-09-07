#' @keywords internal
#' @rdname sys_abort_mtx
sys_abort_mtxKeyformatUnsupported = function(x, name, unsupport_format){
  if(!hasArg(x)){snd:::sys_abort_NoArg(x)}
  if(!hasArg(unsupport_format)){snd:::sys_abort_NoArg(unsupport_format)}
  unsupport_format = stringr::str_flatten(string = paste0('{.code ', unsupport_format, '}'), collapse = ", ")
  custom_message = stringr::str_flatten(paste0('{.code ', snd:::sys_format_support(), '}'), collapse = ", ")

  if(rlang::is_missing(name)){
    snd:::sys_abort(message = c("x" = "Unsupported format in {.arg {arg}}",
                                "!" = "Unsupported format include",
                                "!" = unsupport_format,
                                "i" = "Please specify supported format in {.col @format}",
                                "i" = "Supported format include following:",
                                "i" = custom_message),
                    arg = rlang::caller_arg(arg = x))
  } else {
    snd:::sys_abort(message = c("x" = "Unsupported format in {.mtx {name}}",
                                "!" = "Unsupported format include",
                                "!" = unsupport_format,
                                "i" = "Please specify supported format in {.col @format}",
                                "i" = "Supported format include following:",
                                "i" = custom_message),
                    arg = rlang::caller_arg(arg = x), name = name)
  }
}
