#' @export
formatRI_key2mtx.sndkey_label = function(key, formater, formatee, formaterName, formateeName){
  #Use methods####
  return(snd:::formatRI_key2mtx_sndkey_label(key = key,
                                             formater = formater,
                                             formatee = formatee,
                                             formaterName = formaterName,
                                             formateeName = formateeName))
}

#' @export
formatWO_key2mtx.sndkey_label = function(key, formater, formatee, formaterName, formateeName){
  return(snd:::formatWO_key2mtx_sndkey_label(key = key,
                                             formater = formater,
                                             formatee = formatee,
                                             formaterName = formaterName,
                                             formateeName = formateeName))
}
