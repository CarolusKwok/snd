#' @rdname formatRI_key2mtx
#' @keywords internal
formatRI_key2mtx_sndkey_format = function(key, formater, formatee, formaterName, formateeName){
  UseMethod(generic = "formatRI_key2mtx_sndkey_format", object = formater)
}

#' @export
formatRI_key2mtx_sndkey_format.snd_factor = function(key, formater, formatee, formaterName, formateeName){
  #0. Prep ####
  ava_factor = unique(formater$`@factor`)
  seperated_factor = lapply(X = ava_factor,
                            FUN = function(X, formater){formater = dplyr::filter(formater, `@factor` == X)},
                            formater = formater)

  #1. Check consistent @format across @factor####
  test = sapply(X = seperated_factor,
                FUN = function(X){return(length(unique(X$`@format`)) != 1)})
  if(sum(test)){
    snd:::sys_abort(message = c("x" = "Inconsistent items in {.mtx {formaterName}} {.col @format}, when grouped",
                                "i" = "Grouped by:",
                                "i" = "{.col @factor}",
                                "i" = "Abnormal groups include:",
                                "i" =  snd:::sys_message_columns(columns = ava_factor[test])),
                    formaterName = formaterName)
  }

  #2. Format ####
  use_factor = snd::grab_mtxFactor(formatee)
  format = lapply(X = match(x = use_factor, table = ava_factor, nomatch = 0L),
                  FUN =
                    function(X, seperated_factor){
                      (seperated_factor[[X]]$`@format`) %>%
                        unlist(use.names = FALSE) %>%
                        unique %>%
                        snd:::classify(x = .,
                                       class = paste0("DT", stringr::str_remove(string = .,
                                                                                pattern = "^#")))
                    },
                  seperated_factor = seperated_factor)
  formateeFactor = lapply(X = use_factor,
                          FUN = function(X, formatee){
                            return(unlist(dplyr::select(.data = formatee, {{X}}), use.names = FALSE))
                          },
                          formatee = formatee)
  formated = mapply(FUN = snd:::formatRI_key2mtx_format_use,
                    format = format,
                    colItem = formateeFactor,
                    colName = use_factor,
                    mtxName = formateeName,
                    SIMPLIFY = FALSE, USE.NAMES = FALSE)

  #3. return the formated data back to formatee ###
  for(f in seq_along(use_factor)){
    selName = use_factor[[f]]
    selFormated = formated[[f]]
    formatee = dplyr::mutate(.data = formatee, "{selName}" := selFormated)
  }
  return(list(formater = formater,
              formatee = formatee))
}

#' @export
formatRI_key2mtx_sndkey_format.snd_item = function(key, formater, formatee, formaterName, formateeName){
  #1. Check consistent @format across @factor####
  ava_item = unique(formater$`@item`)
  seperated_item = lapply(X = ava_item,
                          FUN = function(X, formater){formater = dplyr::filter(formater, `@item` == X)},
                          formater = formater)
  test = sapply(X = seperated_item,
                FUN = function(X){return(length(unique(X$`@item`)) != 1)})
  if(sum(test)){
    snd:::sys_abort(message = c("x" = "Inconsistent items in {.mtx {formaterName}} {.col @format}, when grouped",
                                "i" = "Grouped by:",
                                "i" = "{.col @factor}",
                                "i" = "Abnormal groups include:",
                                "i" =  snd:::sys_message_columns(columns = ava_item[test])),
                    formaterName = formaterName)
  }

  #2. Format ####
  use_item = snd::grab_mtxItem(formatee)
  format = lapply(X = match(x = use_item, table = ava_item, nomatch = 0L),
                  FUN = function(X, seperated_item){
                    (seperated_item[[X]]$`@format`) %>%
                      unlist(use.names = FALSE) %>%
                      unique %>%
                      snd:::classify(x = .,
                                     class = paste0("DT",
                                                    stringr::str_remove(string = .,
                                                                        pattern = "^#")))
                  },
                  seperated_item = seperated_item)

  formateeItem = lapply(X = use_item,
                        FUN = function(X, formatee){
                          return(unlist(dplyr::select(.data = formatee, {{X}}), use.names = FALSE))
                        },
                        formatee = formatee)
  formated = mapply(FUN = snd:::formatRI_key2mtx_format_use,
                    format = format,
                    colItem = formateeItem,
                    colName = use_item,
                    mtxName = formateeName,
                    SIMPLIFY = FALSE, USE.NAMES = FALSE)

  #3. return the formated data back to formatee ###
  for(f in seq_along(use_item)){
    selName = use_item[[f]]
    selFormated = formated[[f]]
    formatee = dplyr::mutate(.data = formatee, "{selName}" := selFormated)
  }

  return(list(formater = formater,
              formatee = formatee))
}

#' @rdname formatWO_key2mtx
#' @keywords internal
formatWO_key2mtx_sndkey_format = function(key, formater, formatee, formaterName, formateeName){
  UseMethod(generic = "formatWO_key2mtx_sndkey_format", object = formater)
}

#' @export
formatWO_key2mtx_sndkey_format.snd_factor = function(key, formater, formatee, formaterName, formateeName){
  #0. prep ####
  selected_formater = dplyr::select(.data = formater, `@factor`, `@format`) %>%
    dplyr::distinct()
  ava_factor = unique(selected_formater$`@factor`)
  use_factor = snd:::grab_mtxFactor(formatee)

  #1. Check if the format are consist among @factor####
  test = dplyr::group_by(.data = selected_formater, `@factor`) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::filter(n > 1)
  if(nrow(test)){
    snd:::sys_abort(message = c("x" = "Inconsistent format in {.mtx {formaterName}}",
                                "i" = "Inconsistent format for {.col @factor}",
                                "i" =  snd:::sys_message_code(code = test$`@factor`)),
                    formaterName = formaterName)
  }

  #2. Format ####
  for(colName in use_factor){
    format = unlist(dplyr::filter(selected_formater, `@factor` == colName)$`@format`)
    format = snd:::classify(format,
                            class = paste0("F", stringr::str_remove(string = format, pattern = "^#")))
    colItem = unlist(dplyr::select(.data = formatee, {{colName}}), use.names = FALSE)
    colItem = snd:::formatWO_key2mtx_format_use(format = format,
                                                colItem = colItem,
                                                colName = colName,
                                                mtxName = formateeName,
                                                force = FALSE)
    formatee = dplyr::mutate(.data = formatee, "{colName}" := colItem)
  }

  #3. return the formated data back to formatee####
  return(invisible(list(formater = formater,
                        formatee = formatee)))
}

#' @export
formatWO_key2mtx_sndkey_format.snd_item = function(key, formater, formatee, formaterName, formateeName){
  #0. prep ####
  selected_formater = dplyr::select(.data = formater, `@item`, `@format`) %>%
    dplyr::distinct()
  ava_item = unique(selected_formater$`@item`)
  use_item = snd:::grab_mtxItem(formatee)

  #1. Check if the format are consist among @item####
  test = dplyr::group_by(.data = selected_formater, `@item`) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::filter(n > 1)
  if(nrow(test)){
    snd:::sys_abort(message = c("x" = "Inconsistent format in {.mtx {formaterName}}",
                                "i" = "Inconsistent format for {.col @item}",
                                "i" =  snd:::sys_message_code(code = test$`@item`)),
                    formaterName = formaterName)
  }

  #2. Format ####
  for(colName in use_item){
    format = unlist(dplyr::filter(selected_formater, `@item` == colName)$`@format`)
    format = snd:::classify(format,
                            class = paste0("F", stringr::str_remove(string = format, pattern = "^#")))
    colItem = unlist(dplyr::select(.data = formatee, {{colName}}), use.names = FALSE)
    colItem = snd:::formatWO_key2mtx_format_use(format = format,
                                                colItem = colItem,
                                                colName = colName,
                                                mtxName = formateeName,
                                                force = FALSE)
    formatee = dplyr::mutate(.data = formatee, "{colName}" := colItem)
  }

  #3. return the formated data back to formatee####
  return(invisible(list(formater = formater,
                        formatee = formatee)))
}
