#' @keywords internal
#' @rdname sys_abort_mtx
sys_abort_mtxMissingFactor = function(x, name){
  if(rlang::is_missing(name)){
    snd:::sys_abort(message = c("x" = "Missing factors in {.arg {arg}}",
                                "i" = "Please include factors in {.arg {arg}}"),
                    arg = rlang::caller_arg(arg = x))
  } else {
    snd:::sys_abort(message = c("x" = "Missing factors in {.mtx {name}}",
                                "i" = "Please include factors in {.mtx {name}}"),
                    arg = rlang::caller_arg(arg = x), name = name)
  }
}
