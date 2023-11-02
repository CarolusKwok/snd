#' Title
#'
#' @param key
#' @param formater
#' @param formatee
#' @param formaterName
#' @param formateeName
#'
#' @return
#' @keywords internal
formatWO_key2mtx = function(key, formater, formatee, formaterName, formateeName){
  #Check ####
  if(rlang::is_missing(key)){snd:::sys_abort_NoArg(key)}
  if(rlang::is_missing(formater)){snd:::sys_abort_NoArg(formater)}
  if(rlang::is_missing(formatee)){snd:::sys_abort_NoArg(formatee)}
  if(rlang::is_missing(formaterName)){snd:::sys_abort_NoArg(formaterName)}
  if(rlang::is_missing(formateeName)){snd:::sys_abort_NoArg(formateeName)}
  if(length(key) != 1){snd:::sys_abort_WrongLength(x = key, length = 1L)}
  valid_key = key %in% snd::grab_mtxKey(formater)
  if(!valid_key){snd:::sys_abort_mtxMissingSelectedKey(x = mtx, keys_missing = key, name = formaterName)}

  #Call other functions to format using the keys ####
  key = snd:::classify_key(stringr::str_remove(string = key, pattern = "^@"))
  UseMethod(generic = "formatWO_key2mtx", object = key)
}

formatWO_key2mtx.default = function(key, formater, formatee, formaterName, formateeName){
  return(list(formater = formater,
              formatee = formatee))
}
