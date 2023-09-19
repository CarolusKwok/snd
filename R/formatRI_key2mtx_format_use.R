#' @keywords internal
#' @rdname formatRI_key2mtx
formatRI_key2mtx_format_use = function(format, colItem, colName, mtxName){
  UseMethod(generic = "formatRI_key2mtx_format_use", object = format)
}

#' @keywords internal
#' @rdname formatRI_key2mtx
formatRI_key2mtx_format_use.DTcharacter = function(format, colItem, colName, mtxName){
  #Check ####
  colItem_1 = ifelse(colItem == "#NA", NA, colItem)
  colItem_2 = as.character(colItem_1)
  colItem_test = ifelse(is.na(colItem_1), FALSE, #Do not flag if its designated as #NA
                        is.na(colItem_2)) #flag if its somehow NA
  if(sum(colItem_test)){
    if(rlang::is_missing(mtxName)){
      snd:::sys_abort(message = c("x" = "Describing format incorrectly in {.col {colName}}",
                                  "i" = "Expected format: character"),
                      colName = colName)
    } else {
      snd:::sys_abort(message = c("x" = "Describing format incorrectly in {.col {colName}}, {.mtx {mtxName}}",
                                  "i" = "Expected format: character"),
                      colName = colName, mtxName = mtxName)
    }
  }
  #Return ####
  if(stringr::str_detect(string = format, pattern = "^[#]")){
    return(invisible(as.factor(unname(colItem_2))))
  } else {
    return(invisible(unname(colItem_2)))
  }
}

#' @keywords internal
#' @rdname formatRI_key2mtx
formatRI_key2mtx_format_use.DTnumeric = function(format, colItem, colName, mtxName){
  colItem = unname(colItem)
  #Check ####
  colItem_1 = ifelse(colItem == "#NA", NA, colItem)
  colItem_2 = suppressWarnings(as.numeric(colItem_1))
  colItem_test = ifelse(is.na(colItem_1), FALSE, #Do not flag if its designated as #NA
                        is.na(colItem_2)) #flag if its somehow NA
  if(sum(colItem_test)){
    if(rlang::is_missing(mtxName)){
      snd:::sys_abort(message = c("x" = "Describing format incorrectly in {.col {colName}}",
                                  "i" = "Expected format: numeric"),
                      colName = colName)
    } else {
      snd:::sys_abort(message = c("x" = "Describing format incorrectly in {.col {colName}}, {.mtx {mtxName}}",
                                  "i" = "Expected format: numeric"),
                      colName = colName, mtxName = mtxName)
    }
  }
  #Return ####
  colItem = ifelse(is.na(colItem), 0, colItem_2)
  if(stringr::str_detect(string = format, pattern = "^[#]")){
    return(invisible(as.factor((colItem))))
  } else {
    return(invisible((colItem)))
  }
}

#' @keywords internal
#' @rdname formatRI_key2mtx
formatRI_key2mtx_format_use.DTinteger = function(format, colItem, colName, mtxName){
  colItem = unname(colItem)

  #Check ####
  colItem_1 = ifelse(colItem == "#NA", NA, colItem)
  colItem_num = suppressWarnings(as.numeric(colItem_1))
  colItem_int = suppressWarnings(as.integer(colItem_1))
  colItem_diff= abs(colItem_num - colItem_int)
  colItem_test = ifelse(is.na(colItem_1), FALSE, #Do not flag if its designated as #NA
                 ifelse(is.na(colItem_int), TRUE, (colItem_diff != 0))) #flag if its somehow NA and flag if theres a difference somehow
  if(sum(colItem_test)){
    if(rlang::is_missing(mtxName)){
      snd:::sys_abort(message = c("x" = "Describing format incorrectly in {.col {colName}}",
                                  "i" = "Expected format: integer"),
                      colName = colName)
    } else {
      snd:::sys_abort(message = c("x" = "Describing format incorrectly in {.col {colName}}, {.mtx {mtxName}}",
                                  "i" = "Expected format: integer"),
                      colName = colName, mtxName = mtxName)
    }
  }

  #Return ####
  colItem = ifelse(is.na(colItem), 0L, colItem_int)
  if(stringr::str_detect(string = format, pattern = "^[#]")){
    return(invisible(as.factor(colItem)))
  } else {
    return(invisible(colItem))
  }
}

#' @keywords internal
#' @rdname formatRI_key2mtx
formatRI_key2mtx_format_use.DTlogical = function(format, colItem, colName, mtxName){
  #Check ####
  colItem_test = colItem %in% c("p", NA, "#NA")
  if(!sum(colItem_test)){
    if(rlang::is_missing(mtxName)){
      snd:::sys_abort(message = c("x" = "Describing format incorrectly in {.col {colName}}",
                                  "i" = "Expected format: logical"),
                      colName = colName)
    } else {
      snd:::sys_abort(message = c("x" = "Describing format incorrectly in {.col {colName}}, {.mtx {mtxName}}",
                                  "i" = "Expected format: logical"),
                      colName = colName, mtxName = mtxName)
    }
  }

  #Return ####
  colItem = match(x = colItem, table = c("p", NA, "#NA"))
  colItem = ifelse(colItem == 1, TRUE,
                   ifelse(colItem == 2, FALSE, NA))
  if(stringr::str_detect(string = format, pattern = "^[#]")){
    return(invisible(as.factor(unname(colItem))))
  } else {
    return(invisible(unname(colItem)))
  }
}

#' @keywords internal
#' @rdname formatRI_key2mtx
formatRI_key2mtx_format_use.DTPOSIXct = function(format, colItem, colName, mtxName){
  #Check ####
  colItem_1 = ifelse(colItem == "#NA", NA, colItem)
  colItem_2 = snd::read_ISO8601(time = colItem_1, tzone = "")
  colItem_test = ifelse(is.na(colItem_1), FALSE, #Do not flag if its designated as #NA
                        is.na(colItem_2)) #flag if its somehow NA
  if(sum(colItem_test)){
    if(rlang::is_missing(mtxName)){
      snd:::sys_abort(message = c("x" = "Describing format incorrectly in {.col {colName}}",
                                  "i" = "Expected format: POSIXct"),
                      colName = colName)
    } else {
      snd:::sys_abort(message = c("x" = "Describing format incorrectly in {.col {colName}}, {.mtx {mtxName}}",
                                  "i" = "Expected format: POSIXct"),
                      colName = colName, mtxName = mtxName)
    }
  }
  #Return ####
  if(stringr::str_detect(string = format, pattern = "^[#]")){
    return(invisible(as.factor(colItem_2)))
  } else {
    return(invisible(colItem_2))
  }
}
