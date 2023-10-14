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

#' @export
formatWO_key2mtx.sndkey_factor = function(key, formater, formatee, formaterName, formateeName){
  ava_factor = unique(formater$`@factor`)
  use_factor = snd:::grab_mtxFactor(formatee)

  #Check 1: Check for factor = NA ####
  if(sum(is.na(formater$`@factor`))){
    snd:::sys_abort(message = c("x" = "{.col @factor} contains NA",
                                "i" = "Check {.col @factor} in {.mtx {formaterName}}"),
                    formaterName = formaterName)
  }
  #Check 2: Check for factor used not in mtx factor ####
  if(sum(!(use_factor %in% ava_factor))){
    ava_factor = stringr::str_flatten(string = paste0("{.col ", ava_factor, "}"), collapse = ", ")
    use_factor = stringr::str_flatten(string = paste0("{.col ", use_factor, "}"), collapse = ", ")
    snd:::sys_abort(message = c("x" = "Factor unavailable in {.mtx {formaterName}}",
                                "i" = "Factor available in {.mtx {formaterName}}",
                                "i" = ava_factor,
                                "i" = "Factor used in {.mtx {formateeName}}",
                                "i" = use_factor,
                                "!" = "Unavailable factor in {.mtx {formateeName}}"),
                    formaterName = formaterName,
                    formateeName = formateeName,
                    ava_factor = ava_factor, use_factor = use_factor)
  }
  return(invisible(list(formater = dplyr::mutate(.data = formater,
                                                 `@factor` = as.character(`@factor`)),
                        formatee = formatee)))
}

#' @export
formatWO_key2mtx.sndkey_item = function(key, formater, formatee, formaterName, formateeName){
  ava_item = unique(formater$`@item`)
  use_item = snd:::grab_mtxItem(formatee)

  #Check 1: Check for factor = NA ####
  if(sum(is.na(formater$`@item`))){
    snd:::sys_abort(message = c("x" = "{.col @item} contains NA",
                                "i" = "Check {.col @item} in {.mtx {formaterName}}"),
                    formaterName = formaterName)
  }
  #Check 2: Check for factor used not in mtx factor ####
  if(sum(!(use_item %in% ava_item))){
    ava_item = stringr::str_flatten(string = paste0("{.col ", ava_item, "}"), collapse = ", ")
    use_item = stringr::str_flatten(string = paste0("{.col ", use_item, "}"), collapse = ", ")
    snd:::sys_abort(message = c("x" = "Factor unavailable in {.mtx {formaterName}}",
                                "i" = "Factor available in {.mtx {formaterName}}",
                                "i" = ava_item,
                                "i" = "Factor used in {.mtx {formateeName}}",
                                "i" = use_item,
                                "!" = "Unavailable factor in {.mtx {formateeName}}"),
                    formaterName = formaterName,
                    formateeName = formateeName,
                    ava_item = ava_item, use_item = use_item)
  }
  return(invisible(list(formater = dplyr::mutate(.data = formater,
                                                 `@item` = as.character(`@item`)),
                        formatee = formatee)))
}

#' @export
formatWO_key2mtx.sndkey_type = function(key, formater, formatee, formaterName, formateeName){
  #Check if @type if of the following classes
  keyType_CorrectTest = formater$`@type` %in% c("data", "calc", "stat")

  #If not, type it as "error"
  if(sum(!keyType_CorrectTest)){
    formater = dplyr::mutate(.data = formater,
                             `@type` = ifelse(!keyType_CorrectTest, "error", `@type`))
    snd:::sys_warn(message = c("!" = "Include incorrect {.col @type} in {.mtx {formaterName}}",
                               "i" = "All incorrect types are changed to {.code error}"),
                   formaterName = formaterName)
  }
  return(invisible(list(formater = formater,
                        formatee = formatee)))
}

#' @export
formatWO_key2mtx.sndkey_format = function(key, formater, formatee, formaterName, formateeName){
  #Check if the formats are acceptable ####
  ava_format = unique(formater$`@format`) %>% sort
  supported_format = sort(snd:::sys_format_support()) %>%
    c(., paste0("#", .))
  if(sum(!(ava_format %in% supported_format))){
    snd:::sys_abort(message = c("x" = "Format not supported",
                                "i" = "Supported format:",
                                "i" = stringr::str_flatten(string = supported_format, collapse = ", "),
                                "i" = "Used format in {.mtx {formaterName}}",
                                "i" = stringr::str_flatten(string = ava_format, collapse = ", ")),
                    formaterName = formaterName)
  }

  #Use Methods ####
  return(snd:::formatWO_key2mtx_sndkey_format(key, formater, formatee, formaterName, formateeName))
}

#' @keywords internal
#' @rdname formatWO_key2mtx
formatWO_key2mtx_sndkey_format = function(key, formater, formatee, formaterName, formateeName){
  UseMethod(generic = "formatWO_key2mtx_sndkey_format", object = formater)
}

#' @export
formatWO_key2mtx_sndkey_format.snd_factor = function(key, formater, formatee, formaterName, formateeName){
  #Check if the format are consist among @factor####
  formater_unique = dplyr::select(.data = formater, `@factor`, `@format`) %>%
    dplyr::distinct() %>%
    dplyr::group_by(`@factor`)

  if(dplyr::summarise(.data = formater_unique, n = dplyr::n()) %>%
     dplyr::filter(n > 1) %>%
     nrow){
    snd:::sys_abort(message = c("x" = "Inconsistent format in {.mtx {formaterName}}",
                                "i" = "Inconsistent format for {.col @factor}",
                                "i" =  stringr::str_flatten(formater_unique$`@factor`, collapse = ", ")),
                    formaterName = formaterName)
  }

  #GO! Start to format ####
  use_factor = snd::grab_mtxFactor(formatee)
  for(i in use_factor){
    colName = i
    format = unlist(dplyr::filter(formater_unique, `@factor` == i)$`@format`)
    format = snd:::classify(format,
                            class = paste0("F", stringr::str_remove(string = format, pattern = "^#")))
    colItem = unname(unlist(dplyr::select(.data = formatee, {{colName}})))
    mtxName = formateeName
    colItem = snd:::formatWO_key2mtx_format_use(format = format,
                                                colItem = colItem,
                                                colName = colName,
                                                mtxName = mtxName,
                                                force = FALSE)
    formatee = dplyr::mutate(.data = formatee,
                             "{colName}" := colItem)
  }
  return(invisible(list(formater = formater,
                        formatee = formatee)))
}

#' @export
formatWO_key2mtx_sndkey_format.snd_item = function(key, formater, formatee, formaterName, formateeName){
  #Check if the format are consist among @item####
  formater_unique = dplyr::select(.data = formater, `@item`, `@format`) %>%
    dplyr::distinct() %>%
    dplyr::group_by(`@item`)

  if(dplyr::summarise(.data = formater_unique, n = dplyr::n()) %>%
     dplyr::filter(n > 1) %>%
     nrow){
    snd:::sys_abort(message = c("x" = "Inconsistent format in {.mtx {formaterName}}",
                                "i" = "Inconsistent format for {.col @item}",
                                "i" =  stringr::str_flatten(formater_unique$`@item`, collapse = ", ")),
                    formaterName = formaterName)
  }

  #GO! Start to format ####
  use_item = snd::grab_mtxItem(formatee)
  for(i in use_item){
    colName = i
    format = unlist(dplyr::filter(formater_unique, `@item` == i)$`@format`)
    format = snd:::classify(format,
                            class = paste0("F", stringr::str_remove(string = format, pattern = "^#")))
    colItem = unname(unlist(dplyr::select(.data = formatee, {{colName}})))
    mtxName = formateeName
    colItem = snd:::formatWO_key2mtx_format_use(format = format,
                                                colItem = colItem,
                                                colName = colName,
                                                mtxName = mtxName,
                                                force = FALSE)
    formatee = dplyr::mutate(.data = formatee,
                             "{colName}" := colItem)
  }
  return(invisible(list(formater = formater,
                        formatee = formatee)))
}

#' @export
formatWO_key2mtx.sndkey_label = function(key, formater, formatee, formaterName, formateeName){
  return(snd:::formatWO_key2mtx_sndkey_label(key, formater, formatee, formaterName, formateeName))
}

#' @keywords internal
#' @rdname formatWO_key2mtx
formatWO_key2mtx_sndkey_label = function(key, formater, formatee, formaterName, formateeName){
  UseMethod(generic = "formatWO_key2mtx_sndkey_label", object = formater)
}

#' @export
formatWO_key2mtx_sndkey_label.snd_factor = function(key, formater, formatee, formaterName, formateeName){
  use_factor = snd:::grab_mtxFactor(dataframe = formatee)
  ava_label = lapply(X = use_factor,
                     FUN = function(X, formater){
                       label = dplyr::filter(.data = formater, `@factor` == X) %>%
                         dplyr::select(`@label`) %>%
                         unlist %>%
                         unname %>%
                         ifelse(is.na(.), "#NA", .)
                       return(label)
                     }, formater = formater)
  use_label = lapply(X = use_factor,
                     FUN = function(X, formatee){
                       formatee %>%
                         dplyr::select({{X}}) %>%
                         unlist %>% unname %>%
                         return
                     }, formatee = formatee)
  #the actual test ####
  test = mapply(FUN =
                  function(ava_label, use_label){
                    if("###" %in% ava_label){return(0)} else {return(sum(!(use_label %in% ava_label)))}
                  }, ava_label = ava_label, use_label = use_label, SIMPLIFY = TRUE, USE.NAMES = FALSE)
  if(sum(test)){
    failed = stringr::str_flatten(string = paste0("{.col ", use_label[as.logical(test)], "}"), collapse = ", ")
    snd:::sys_abort(message = c("x" = "Column not described in {.col @label}",
                                "!" = "Columns failed to describe by {.mtx {formaterName}} in {.mtx {formateeName}}",
                                "i" = "Poorly descibed factors include:",
                                "i" = failed),
                    formaterName = formaterName, formateeName = formateeName)
  }

  #return everything ####
  return(invisible(list(formater = dplyr::mutate(.data = formater,
                                                 `@label` = ifelse(is.na(`@label`), "#NA", `@label`)),
                        formatee = formatee)))
}

#' @export
formatWO_key2mtx_sndkey_label.snd_item = function(key, formater, formatee, formaterName, formateeName){
  use_item = snd:::grab_mtxItem(dataframe = formatee)
  ava_label = lapply(X = use_item,
                     FUN = function(X, formater){
                       label = dplyr::filter(.data = formater, `@item` == X) %>%
                         dplyr::select(`@label`) %>%
                         unlist %>%
                         unname %>%
                         ifelse(is.na(.), "#NA", .)
                       return(label)
                     }, formater = formater)
  use_label = lapply(X = use_item,
                     FUN = function(X, formatee){
                       formatee %>%
                         dplyr::select({{X}}) %>%
                         unlist %>% unname %>%
                         return
                     }, formatee = formatee)
  #the actual test ####
  test = mapply(FUN =
                  function(ava_label, use_label){
                    if("###" %in% ava_label){return(0)} else {return(sum(!(use_label %in% ava_label)))}
                  }, ava_label = ava_label, use_label = use_label, SIMPLIFY = TRUE, USE.NAMES = FALSE)
  if(sum(test)){
    failed = stringr::str_flatten(string = paste0("{.col ", use_label[as.logical(test)], "}"), collapse = ", ")
    snd:::sys_abort(message = c("x" = "Column not described in {.col @label}",
                                "!" = "Columns failed to describe by {.mtx {formaterName}} in {.mtx {formateeName}}",
                                "i" = "Poorly descibed factors include:",
                                "i" = failed),
                    formaterName = formaterName, formateeName = formateeName)
  }

  #return everything ####
  return(invisible(list(formater = dplyr::mutate(.data = formater,
                                                 `@label` = ifelse(is.na(`@label`), "#NA", `@label`)),
                        formatee = formatee)))
}
