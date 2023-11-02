#' @keywords internal
format_sndkey_item_engine = function(key, formater, formatee, formaterName, formateeName){
  ava_item = unique(formater$`@item`)
  use_item = snd::grab_mtxItem(formatee)

  #1. Check if @item is all filled ####
  test = sum(is.na(ava_item) | ava_item == "#NA")
  if(test){
    snd:::sys_abort(message = c("x" = "{.col @item} not filled in {.mtx {formaterName}}",
                                "i" = "{.col @item} must be all filled to function properly",
                                "i" = "{.code #NA} is not accepted"),
                    formaterName = formaterName)
  }
  #2. Check if @item contains factors ####
  test = stringr::str_detect(string = ava_item, pattern = "^#")
  if(sum(test)){
    snd:::sys_abort(message = c("x" = "{.mtx {formaterName}} contain factors in {.col @item}",
                                "!" = "Factors include:",
                                "i" = snd:::sys_message_code(code = unique(ava_item[test]))),
                    formaterName = formaterName)
  }
  #3. Checks if the formatee described ####
  test = !(use_item %in% ava_item)
  if(sum(test)){
    snd:::sys_abort(message = c("x" = "Missing items in {.mtx {formaterName}} {.col @item}",
                                "i" = "Items missing:",
                                "i" = snd:::sys_message_columns(columns = unique(use_item[test]))),
                    formaterName = formaterName)
  }
  #Return ####
  return(list(formater = formater,
              formatee = formatee))
}


#' @export
formatRI_key2mtx.sndkey_item = function(key, formater, formatee, formaterName, formateeName){
  return(snd:::format_sndkey_item_engine(key = key,
                                         formater = formater,
                                         formatee = formatee,
                                         formaterName = formaterName,
                                         formateeName = formateeName))
}

#' @export
formatWO_key2mtx.sndkey_item = function(key, formater, formatee, formaterName, formateeName){
  return(snd:::format_sndkey_item_engine(key = key,
                                         formater = formater,
                                         formatee = formatee,
                                         formaterName = formaterName,
                                         formateeName = formateeName))
}
