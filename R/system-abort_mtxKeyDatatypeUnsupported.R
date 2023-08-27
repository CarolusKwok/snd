#' @keywords internal
#' @rdname sys_abort_mtx
sys_abort_mtxKeyDatatypeUnsupported = function(x, name, unsupport_datatype){
  if(!hasArg(x)){snd:::sys_abort_NoArg(x)}
  if(!hasArg(unsupport_datatype)){snd:::sys_abort_NoArg(unsupport_datatype)}
  unsupport_datatype = stringr::str_flatten(string = paste0('{.code ', unsupport_datatype, '}'), collapse = ", ")
  custom_message = stringr::str_flatten(paste0('{.code ', snd:::sys_datatype_support(), '}'), collapse = ", ")

  if(rlang::is_missing(name)){
    snd:::sys_abort(message = c("x" = "Unsupported datatype in {.arg {arg}}",
                                "!" = "Unsupported datatype include",
                                "!" = unsupport_datatype,
                                "i" = "Please specify supported datatype in {.col @datatype}",
                                "i" = "Supported datatype include following:",
                                "i" = custom_message),
                    arg = rlang::caller_arg(arg = x))
  } else {
    snd:::sys_abort(message = c("x" = "Unsupported datatype in {.mtx {name}}",
                                "!" = "Unsupported datatype include",
                                "!" = unsupport_datatype,
                                "i" = "Please specify supported datatype in {.col @datatype}",
                                "i" = "Supported datatype include following:",
                                "i" = custom_message),
                    arg = rlang::caller_arg(arg = x), name = name)
  }
}
