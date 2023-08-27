#' @keywords internal
#' @rdname sys_abort_mtx
sys_abort_mtxMissingItem = function(x, name){
  if(rlang::is_missing(name)){
    snd:::sys_abort(message = c("x" = "Missing items in {.arg {arg}}",
                                "i" = "Please include items in {.arg {arg}}"),
                    arg = rlang::caller_arg(arg = x))
  } else {
    snd:::sys_abort(message = c("x" = "Missing items in {.mtx {name}}",
                                "i" = "Please include items in {.mtx {name}}"),
                    arg = rlang::caller_arg(arg = x), name = name)
  }
}
