#' @keywords internal
#' @rdname sys_abort_mtx
sys_abort_mtxMissingSelectedItem = function(x, item_missing, name){
  if(rlang::is_missing(name)){
    snd:::sys_abort(message = c("x" = "Missing items in {.arg {arg}}",
                                "i" = "Please include items in {.arg {arg}}",
                                "i" = "items missing: {.col {item_missing}}"),
                    item_missing = item_missing, arg = rlang::caller_arg(x))
  } else {
    snd:::sys_abort(message = c("x" = "Missing items in {.mtx {name}}",
                                "i" = "Please include items in {.mtx {name}}",
                                "i" = "items missing: {.col {item_missing}}"),
                    name = name, item_missing = item_missing, arg = rlang::caller_arg(x))
  }
}
