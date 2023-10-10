#' @keywords internal
#' @rdname sys_abort_mtx
sys_abort_mtxMissingSelectedKey = function(x, keys_missing, name){
  if(rlang::is_missing(x)){snd:::sys_abort_NoArg(x)}
  if(rlang::is_missing(keys_missing)){snd:::sys_abort_NoArg(keys_missing)}
  if(rlang::is_missing(name)){snd:::sys_abort_NoArg(name)}

  snd:::sys_abort(message = c("x" = "Missing keys in {.mtx {name}}",
                              "i" = "Please include keys in {.mtx {name}}",
                              "i" = "Keys missing: {.col {keys_missing}}"),
                  name = name, keys_missing = keys_missing)
}

#Used in checkRW_matrix, formatRI_key1mtx, formatRI_key2mtx, formatWO_key1mtx, formatWO_key2mtx,
