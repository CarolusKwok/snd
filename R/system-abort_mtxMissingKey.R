#' @keywords internal
#' @rdname sys_abort_mtx
sys_abort_mtxMissingKey = function(x, keys_missing, name){
  if(rlang::is_missing(name)){
    snd:::sys_abort(message = c("x" = "Missing keys in {.arg {arg}}",
                                "i" = "Please include keys in {.arg {arg}}",
                                "i" = "Keys missing: {.col {keys_missing}}"),
                    keys_missing = keys_missing, arg = rlang::caller_arg(x))
  } else {
    snd:::sys_abort(message = c("x" = "Missing keys in {.mtx {name}}",
                                "i" = "Please include keys in {.mtx {name}}",
                                "i" = "Keys missing: {.col {keys_missing}}"),
                    name = name,
                    keys_missing = keys_missing)
  }
}
