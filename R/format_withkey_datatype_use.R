#' @keywords internal
#' @rdname format_key2mtx
format_key2mtx_datatype_use = function(colItem, colName, mtxName){
  UseMethod(generic = "format_key2mtx_datatype_use", object = colItem)
}

#' @keywords internal
#' @rdname format_key2mtx
format_key2mtx_datatype_use.DTnumeric = function(colItem, colName, mtxName){
  #Check ####
  test = tibble::tibble(colItem = ifelse(colItem == "#NA", NA, colItem),
                        colItem_num = suppressWarnings(as.numeric(colItem))) %>%
    dplyr::mutate(test1 = (is.na(colItem) & is.na(colItem_num)),
                  test2 = !is.na(colItem_num),
                  test = test1 | test2)
  if(sum(!test$test)){
    if(rlang::is_missing(mtxName)){
      snd:::sys_abort(message = c("x" = "Describing datatype incorrectly in {.col {colName}}",
                                  "i" = "Expected datatype: numeric"),
                      colName = colName)
    } else {
      snd:::sys_abort(message = c("x" = "Describing datatype incorrectly in {.col {colName}}, {.mtx {mtxName}}",
                                  "i" = "Expected datatype: numeric"),
                      colName = colName, mtxName = mtxName)
    }
  }

  #Return ####
  return(unclass(test$colItem_num))
}

#' @keywords internal
#' @rdname format_key2mtx
format_key2mtx_datatype_use.DTlogical = function(colItem, colName, mtxName){
  #Check ####
  colItem_test = colItem %in% c("p", NA, "#NA")
  if(!sum(colItem_test)){
    if(rlang::is_missing(mtxName)){
      snd:::sys_abort(message = c("x" = "Describing datatype incorrectly in {.col {colName}}",
                                  "i" = "Expected datatype: logical"),
                      colName = colName)
    } else {
      snd:::sys_abort(message = c("x" = "Describing datatype incorrectly in {.col {colName}}, {.mtx {mtxName}}",
                                  "i" = "Expected datatype: logical"),
                      colName = colName, mtxName = mtxName)
    }
  }

  #Return ####
  colItem = match(x = colItem, table = c("p", NA, "#NA"))
  colItem = ifelse(colItem == 1, TRUE,
                   ifelse(colItem == 2, FALSE, NA))
  return(colItem)
}

#' @keywords internal
#' @rdname format_key2mtx
format_key2mtx_datatype_use.DTinteger = function(colItem, colName, mtxName){
  #Check ####
  colItem = ifelse(colItem == "#NA", NA, colItem)
  colItem_num = suppressWarnings(as.numeric(colItem))
  colItem_int = suppressWarnings(as.integer(colItem))
  test1 = (is.na(colItem) & is.na(colItem_num))
  test2 = (colItem_num == colItem_int)
  test2 = ifelse(is.na(test2), FALSE, test2)
  test = test1 | test2
  if(sum(!test)){
    if(rlang::is_missing(mtxName)){
      snd:::sys_abort(message = c("x" = "Describing datatype incorrectly in {.col {colName}}",
                                  "i" = "Expected datatype: integer"),
                      colName = colName)
    } else {
      snd:::sys_abort(message = c("x" = "Describing datatype incorrectly in {.col {colName}}, {.mtx {mtxName}}",
                                  "i" = "Expected datatype: integer"),
                      colName = colName, mtxName = mtxName)
    }
  }

  #Return ####
  return(invisible(colItem_int))
}

#' @keywords internal
#' @rdname format_key2mtx
format_key2mtx_datatype_use.DTcharacter = function(colItem, colName, mtxName){
  #Check ####
  colItem = ifelse(colItem == "#NA", NA, colItem)
  colItem_test = is.character(colItem)
  if(sum(!colItem_test)){
    if(rlang::is_missing(mtxName)){
      snd:::sys_abort(message = c("x" = "Describing datatype incorrectly in {.col {colName}}",
                                  "i" = "Expected datatype: character"),
                      colName = colName)
    } else {
      snd:::sys_abort(message = c("x" = "Describing datatype incorrectly in {.col {colName}}, {.mtx {mtxName}}",
                                  "i" = "Expected datatype: character"),
                      colName = colName, mtxName = mtxName)
    }
  }
  #Return ####
  return(invisible(colItem))
}

#' @keywords internal
#' @rdname format_key2mtx
format_key2mtx_datatype_use.DTPOSIXct = function(colItem, colName, mtxName){
  #Check ####
  test = tibble::tibble(Item = unclass(colItem)) %>%
    dplyr::mutate(time = as.POSIXct(x = Item, tryFormats = c("%Y%m%d-%H%M%OS", "%Y%m%d-%H%M")),
                  test_Item = is.na(Item),
                  test_time = is.na(time),
                  test = ifelse(test_Item, TRUE, ifelse(test_time, FALSE, TRUE)))
  if(sum(!test$test)){
    if(rlang::is_missing(mtxName)){
      snd:::sys_abort(message = c("x" = "Describing datatype incorrectly in {.col {colName}}",
                                  "i" = "Expected datatype: POSIXct"),
                      colName = colName)
    } else {
      snd:::sys_abort(message = c("x" = "Describing datatype incorrectly in {.col {colName}}, {.mtx {mtxName}}",
                                  "i" = "Expected datatype: POSIXct"),
                      colName = colName, mtxName = mtxName)
    }
  }
  #Return ####
  return(invisible(test$time))
}

#' @keywords internal
#' @rdname format_key2mtx
format_key2mtx_datatype_use.DTfactor = function(colItem, colName, mtxName){
  #Check ####
  colItem = ifelse(colItem == "#NA", NA, colItem)
  colItem_test = is.character(colItem)
  if(sum(!colItem_test)){
    if(rlang::is_missing(mtxName)){
      snd:::sys_abort(message = c("x" = "Describing datatype incorrectly in {.col {colName}}",
                                  "i" = "Expected datatype: factor"),
                      colName = colName)
    } else {
      snd:::sys_abort(message = c("x" = "Describing datatype incorrectly in {.col {colName}}, {.mtx {mtxName}}",
                                  "i" = "Expected datatype: factor"),
                      colName = colName, mtxName = mtxName)
    }
  }
  #Return ####
  return(invisible(as.factor(colItem)))
}

#' @keywords internal
#' @rdname format_key2mtx
format_key2mtx_datatype_use.DTflexible = function(colItem, colName, mtxName){
  return(invisible(colItem))
}
