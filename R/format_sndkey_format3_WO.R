#' @keywords internal
#' @rdname formatWO_key2mtx
formatWO_key2mtx_format_use = function(format, colItem, colName, mtxName, force = FALSE){
  UseMethod(generic = "formatWO_key2mtx_format_use", object = format)
}

#' @export
formatWO_key2mtx_format_use.Fcharacter = function(format, colItem, colName, mtxName, force = FALSE){
  colItem = ifelse(is.na(colItem), NA, as.character(colItem))
  return(colItem)
}

#' @export
formatWO_key2mtx_format_use.Finteger = function(format, colItem, colName, mtxName, force = FALSE){
  colItem = ifelse(is.na(colItem), "#NA",
                   ifelse(colItem == 0, NA, as.character(colItem)))
  return(colItem)
}

#' @export
formatWO_key2mtx_format_use.Flogical = function(format, colItem, colName, mtxName, force = FALSE){
  logical = ifelse(colItem, "p", ifelse(!colItem, NA, "#NA"))
}

#' @export
formatWO_key2mtx_format_use.Fnumeric = function(format, colItem, colName, mtxName, force = FALSE){
  colItem = ifelse(is.na(colItem), "#NA",
                   ifelse(colItem == 0, NA, as.character(colItem)))
  return(colItem)
}

#' @export
formatWO_key2mtx_format_use.FPOSIXct = function(format, colItem, colName, mtxName, force = FALSE){
  if(stringr::str_detect(string = unclass(format), pattern = "^#")){
    colItem = as.POSIXct(colItem)
  }
  colItem = snd::write_ISO8601(time = colItem)
  return(colItem)
}
