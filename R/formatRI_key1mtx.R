#' System tools: Checking and Formatting the keys during read-in.
#'
#' @description
#' In SND, a `key` is a column within the matrix that can be used to check and modify itself, it's matrix and other matrixes. This function checks and modifies it's own matrix only, using a key. To modify other matrixes, check the function `snd:::formatRI_key2mtx`. To enhance security, only 1 key will be accepted.
#'
#' @param key The key in `character`. Only 1 key will be accepted.
#' @param mtx The matrix in `data.frame`. The matrix must contain the key.
#' @param mtxName The name of the `mtx` in character.
#'
#' @return A matrix, checked (or/and modified) by the key.
#' @keywords internal
#' @examples
formatRI_key = function(key, mtx, mtxName){
  #Check ####
  if(rlang::is_missing(key)){snd:::sys_abort_NoArg(key)}
  if(rlang::is_missing(mtx)){snd:::sys_abort_NoArg(mtx)}
  if(rlang::is_missing(mtxName)){snd:::sys_abort_NoArg(mtxName)}
  if(length(key) != 1){snd:::sys_abort_WrongLength(x = key, length = 1L)}
  if(!(key %in% snd::grab_mtxKey(mtx))){
    snd:::sys_abort_mtxMissingSelectedKey(x = mtx, keys_missing = key, name = mtxName)
  }
  #Call other functions to format using the keys ####
  key = snd:::classify_key(stringr::str_remove(string = key, pattern = "^@"))
  UseMethod(generic = "formatRI_key", object = key)
}

#' @export
formatRI_key.default = function(key, mtx, mtxName){
  return(invisible(mtx))
}
