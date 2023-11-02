#' @keywords internal
#' @rdname formatRI_key2mtx
formatRI_key2mtx_format_use = function(format, colItem, colName, mtxName, force = FALSE){
  supportedFormat = snd:::sys_format_support(with_abbr = TRUE)
  selFormat = unlist(stringr::str_remove(string = class(format), pattern = "^DT"))[[1]]
  defValue = unname(unlist(dplyr::filter(.data = supportedFormat,
                                         full == selFormat)$default))
  colItem = ifelse(is.na(colItem), defValue, colItem)

  return(snd:::formatRI_key2mtx_format_use2(format = format,
                                            colItem = colItem,
                                            colName = colName,
                                            mtxName = mtxName,
                                            force = force))
}

#' @keywords internal
#' @rdname formatRI_key2mtx
formatRI_key2mtx_format_use2 = function(format, colItem, colName, mtxName, force = FALSE){
  UseMethod(generic = "formatRI_key2mtx_format_use2", object = format)
}

#' @export
formatRI_key2mtx_format_use2.DTcharacter = function(format, colItem, colName, mtxName, force = FALSE){
  colItem = unname(colItem)
  colItem_1 = ifelse(colItem == "#NA", NA, colItem)
  colItem_2 = suppressWarnings(as.character(colItem_1))
  if(!force){
    #Check ####
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
  }
  #Return ####
  if(stringr::str_detect(string = format, pattern = "^#")){
    return(invisible(as.factor(colItem_2)))
  } else {
    return(invisible(colItem_2))
  }
}

#' @export
formatRI_key2mtx_format_use2.DTnumeric = function(format, colItem, colName, mtxName, force = FALSE){
  colItem = unname(colItem)
  colItem_1 = ifelse(colItem == "#NA", NA, colItem)
  colItem_2 = suppressWarnings(as.numeric(colItem_1))
  if(!force){
    #Check ####
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
  }
  #Return ####
  if(stringr::str_detect(string = format, pattern = "^#")){
    return(invisible(as.factor(colItem_2)))
  } else {
    return(invisible(colItem_2))
  }
}

#' @export
formatRI_key2mtx_format_use2.DTinteger = function(format, colItem, colName, mtxName, force = FALSE){
  colItem = unname(colItem)
  colItem_1 = ifelse(colItem == "#NA", NA, colItem)
  colItem_num = suppressWarnings(as.numeric(colItem_1))
  colItem_int = suppressWarnings(as.integer(colItem_1))
  colItem_diff= abs(colItem_num - colItem_int)
  colItem_int = ifelse(colItem_diff == 0, colItem_int, NA)

  if(!force){
    #Check ####
    colItem_test = ifelse(is.na(colItem_1), FALSE, #Do not flag if its designated as #NA
                          is.na(colItem_int)) #flag if its somehow NA
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
  }

  #Return ####
  if(stringr::str_detect(string = format, pattern = "^#")){
    return(invisible(as.factor(colItem_int)))
  } else {
    return(invisible(colItem_int))
  }
}

#' @export
formatRI_key2mtx_format_use2.DTlogical = function(format, colItem, colName, mtxName, force = FALSE){
  colItem = unname(colItem)
  colItem_1 = ifelse(colItem == "#NA", NA, colItem)
  colItem_2 = suppressWarnings(ifelse(colItem_1 == "p", TRUE, ifelse(colItem_1 == "n", FALSE, NA)))
  if(!force){
    #Check ####
    colItem_test = ifelse(is.na(colItem_1), FALSE, #Do not flag if its designated as #NA
                          is.na(colItem_2)) #flag if its somehow NA

    if(sum(colItem_test)){
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
  }
  #Return ####
  if(stringr::str_detect(string = format, pattern = "^#")){
    return(invisible(as.factor(colItem_2)))
  } else {
    return(invisible(colItem_2))
  }
}

#' @export
formatRI_key2mtx_format_use2.DTPOSIXct = function(format, colItem, colName, mtxName, force = FALSE){
  colItem = unname(colItem)
  colItem_1 = ifelse(colItem == "#NA", NA, colItem)
  colItem_2 = snd::read_ISO8601(time = colItem_1, tzone = "")

  if(!force){
    #Check ####
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
  }

  #Return ####
  if(stringr::str_detect(string = format, pattern = "^#")){
    return(invisible(as.factor(colItem_2)))
  } else {
    return(invisible(colItem_2))
  }
}
