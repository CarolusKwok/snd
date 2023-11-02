#' @keywords internal
format_sndkey_type_engine = function(key, mtx, mtxName){
  #Check if @type is of the following classes.
  #If not, type it as "error"
  support = c("data", "calc", "stat")
  test = !(snd::grab_keyHead(mtx = mtx, key = "@type") %in% support)
  if(sum(test)){
    snd:::sys_warn(message = c("!" = "Incorrect {.col @type} in {.mtx {mtxName}}",
                               "i" = "Supported {.col @type}:",
                               "i" = snd:::sys_message_code(code = support),
                               "i" = "Failed {.col @type}:",
                               "i" = snd:::sys_message_code(code = unique((mtx$`@type`)[test])),
                               "!" = "All incorrect types are changed to {.code error}"),
                   mtxName = mtxName)
    mtx = dplyr::mutate(.data = mtx,
                        `@type` = ifelse(test, "error", `@type`))
  }
  return(invisible(mtx))
}

#' @export
formatRI_key.sndkey_type = function(key, mtx, mtxName){
  return(invisible(snd:::format_sndkey_type_engine(key = key,
                                                   mtx = mtx,
                                                   mtxName = mtxName)))
}

#' @export
formatWO_key.sndkey_type = function(key, mtx, mtxName){
  return(invisible(snd:::format_sndkey_type_engine(key = key,
                                                   mtx = mtx,
                                                   mtxName = mtxName)))
}
