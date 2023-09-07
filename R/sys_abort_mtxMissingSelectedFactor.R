#' @keywords internal
#' @rdname sys_abort_mtx
sys_abort_mtxMissingSelectedFactor = function(x, factor_missing, name){
  if(rlang::is_missing(name)){
    snd:::sys_abort(message = c("x" = "Missing factors in {.arg {arg}}",
                                "i" = "Please include factors in {.arg {arg}}",
                                "i" = "factors missing: {.col {factor_missing}}"),
                    factor_missing = factor_missing, arg = rlang::caller_arg(x))
  } else {
    snd:::sys_abort(message = c("x" = "Missing factors in {.mtx {name}}",
                                "i" = "Please include factors in {.mtx {name}}",
                                "i" = "factors missing: {.col {factor_missing}}"),
                    name = name, factor_missing = factor_missing)
  }
}
