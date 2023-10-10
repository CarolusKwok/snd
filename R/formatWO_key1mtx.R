#' Title
#'
#' @param key
#' @param mtx
#' @param mtxName
#'
#' @return A matrix, checked (or/and modified) by the key.
#' @keywords internal
formatWO_key = function(key, mtx, mtxName){
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
  UseMethod(generic = "formatWO_key", object = key)
}

#' @export
formatWO_key.sndkey_type = function(key, mtx, mtxName){
  #Check if @type is of the following classes.
  #If not, type it as "error"
  keyType_CorrectTest = mtx$`@type` %in% c("data", "calc", "stat")
  if(sum(!keyType_CorrectTest)){
    mtx = dplyr::mutate(.data = mtx,
                        `@type` = ifelse(!keyType_CorrectTest, "error", `@type`))
    snd:::sys_warn(message = c("!" = "Include incorrect {.col @type} in {.mtx {mtxName}}",
                               "i" = "All incorrect types are changed to {.code error}"),
                   mtxName = mtxName)
  }
  return(invisible(mtx))
}
