#' Testing
#'
#' @param formater
#' @param formatee
#'
#' @return
#' @keywords internal
#' @rdname format_withkey
format_withkey = function(key, formater, formatee, formateeName){
  key = stringr::str_sub(string = key, start = 2L, end = -1L)
  fun_text = paste0("snd:::format_withkey_", key, "(formater = formater, formatee = formatee, formateeName = formateeName)")
  return(eval(parse(text = fun_text)))
}

#' @keywords internal
#' @rdname format_withkey
format_withkey_factor = function(formater, formatee, formateeName){
  return(formatee)
}

#' @keywords internal
#' @rdname format_withkey
format_withkey_item = function(formater, formatee, formateeName){
  return(formatee)
}

#' @keywords internal
#' @rdname format_withkey
format_withkey_type = function(formater, formatee, formateeName){
  return(formatee)
}

#' @keywords internal
#' @rdname format_withkey
format_withkey_label = function(formater, formatee, formateeName){
  #Check ####
  if(snd:::is_snd_factor(formater)){
    selected = tibble::tibble(elements = formater$`@factor`,
                              labels = formater$`@label`)
  }
  if(snd:::is_snd_item(formater)){
    selected = tibble::tibble(elements = formater$`@item`,
                              labels = formater$`@label`)
  }

  unique_elements = unique(selected$elements)
  used_elements = unique_elements[unique_elements %in% colnames(formatee)]
  seperated_selected = lapply(X   = used_elements,
                              FUN = function(X, df){return(dplyr::filter(.data = df, elements == X)$labels)},
                              df  = selected)

  test = vector(length = length(used_elements)) #TRUE = pass; FALSE = fail
  for(i in 1:length(used_elements)){
    selected_formatee = dplyr::select(.data = formatee, input = !!rlang::sym({{used_elements[i]}}))
    expected_input = unlist(seperated_selected[i])
    selected_formatee = dplyr::mutate(.data = selected_formatee,
                                      in_expected = input %in% expected_input,
                                      in_expected = ifelse("###" %in% expected_input, TRUE, in_expected))
    test[i] = (sum(selected_formatee$in_expected) == nrow(selected_formatee))
  }

  if(sum(!test)){
    failed_columns = used_elements[!test]
    failed_columns = stringr::str_flatten(string = paste0("{.col ", failed_columns, "}"), collapse = ", ")
    if(rlang::is_missing(formateeName)){
      snd:::sys_abort(message = c("x" = "Describing columns incorrectly in {.arg {arg}}",
                                  "!" = "Incorrectly described columns:",
                                  "!" = failed_columns,
                                  "i" = "Please check {.arg {arg}}"),
                      arg = rlang::caller_arg(arg = formatee))
    } else {
      snd:::sys_abort(message = c("x" = "Describing columns incorrectly in {.mtx {formateeName}}",
                                  "!" = "Incorrectly described columns:",
                                  "!" = failed_columns,
                                  "i" = "Please check {.mtx {formateeName}}"),
                      arg = rlang::caller_arg(arg = formatee), formateeName = formateeName)
    }
  }
  return(invisible(formatee))
}

#' @keywords internal
#' @rdname format_withkey
format_withkey_datatype = function(formater, formatee, formateeName){
  if(snd::is_snd_item(formater)){
    use_item = snd:::sys_grab_dfItem(formatee)
    for(i in use_item){
      sel_ColItem = unlist(dplyr::select(.data = formatee, {{i}}))
      sel_Datatype = dplyr::filter(formater, `@item` == i)$`@datatype`
      class(sel_ColItem) = paste0("DT", sel_Datatype)
      sel_ColItem = snd:::format_withkey_datatype_use(colItem = sel_ColItem,
                                                      colName = i,
                                                      mtxName = formateeName)
      formatee = dplyr::mutate(.data = formatee, "{i}" := sel_ColItem)
    }
  }
  if(snd::is_snd_factor(formater)){
    use_factor = snd:::sys_grab_dfFactor(formatee)
    for(i in use_factor){
      Datatype = snd:::sys_grab_keyHead(mtx = dplyr::filter(formater, `@factor` == i),
                                        key = "@datatype")
      colItem = unlist(dplyr::select(.data = formatee, {{i}}))
      class(colItem) = paste0("DT", unique(Datatype))
      colItem = snd:::format_withkey_datatype_use(colItem = colItem,
                                                  colName = i,
                                                  mtxName = formateeName)
      formatee = dplyr::mutate(.data = formatee, "{i}" := colItem)
    }
  }
  return(invisible(formatee))
}

#' @keywords internal
#' @rdname format_withkey
format_withkey_datatype_use = function(colItem, colName, mtxName){
  UseMethod(generic = "format_withkey_datatype_use", object = colItem)
}

#' @keywords internal
#' @rdname format_withkey
format_withkey_datatype_use.DTnumeric = function(colItem, colName, mtxName){
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
#' @rdname format_withkey
format_withkey_datatype_use.DTlogical = function(colItem, colName, mtxName){
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
#' @rdname format_withkey
format_withkey_datatype_use.DTinteger = function(colItem, colName, mtxName){
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
#' @rdname format_withkey
format_withkey_datatype_use.DTcharacter = function(colItem, colName, mtxName){
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
#' @rdname format_withkey
format_withkey_datatype_use.DTPOSIXct = function(colItem, colName, mtxName){
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
#' @rdname format_withkey
format_withkey_datatype_use.DTfactor = function(colItem, colName, mtxName){
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
#' @rdname format_withkey
format_withkey_datatype_use.DTflexible = function(colItem, colName, mtxName){
  return(invisible(colItem))
}
