#' System tools: Checking and Formatting other matrixs using keys during read-in.
#'
#' @description
#' In SND, a `key` is a column within the matrix that can be used to check and modify itself, it's matrix and other matrices. This function uses a key within `formater`, to checks and modifies another matrix `formatee`, using a key. To modify it's own matrix, check the function `snd:::formatRI_key`. To enhance security, only 1 key will be accepted.
#'
#' @param key The key in `character`. Only 1 key will be accepted.
#' @param formater The matrix in `data.frame` to format the formatee. The matrix must contain the key.
#' @param formatee The matrix in `data.frame` to be formatted using formater's keys.
#' @param formateeName The name of the formatee in `character`.
#'
#' @return The formatee matrix, checked (and formatted) by formater matrix.
#' @export
formatRI_key2mtx = function(key, formater, formatee, formateeName){
  #Check ####
  if(rlang::is_missing(key)){snd:::sys_abort_NoArg(key)}
  if(rlang::is_missing(formater)){snd:::sys_abort_NoArg(formater)}
  if(rlang::is_missing(formatee)){snd:::sys_abort_NoArg(formatee)}
  if(length(key) != 1){snd:::sys_abort_WrongLength(x = key, length = 1L)}
  valid_key = key %in% snd::grab_mtxKey(formater)
  if(!valid_key){snd:::sys_abort_mtxMissingKey(x = mtx, keys_missing = key, name = mtxName)}

  #Call other functions to format using the keys ####
  key = snd:::classify_key(stringr::str_remove(string = key, pattern = "^[@]"))
  UseMethod(generic = "formatRI_key2mtx", object = key)
}

#' @export
formatRI_key2mtx.sndkey_type = function(key, formater, formatee, formateeName){
  #type doesn't interact to other matrixes
  #Return ####
  return(formatee)
}

#' @export
formatRI_key2mtx.sndkey_factor = function(key, formater, formatee, formateeName){
  #Factor checks if the formatee is described
  use_factor = snd::grab_mtxFactor(formatee)
  test = !(use_factor %in% unique(formater$`@factor`))
  if(sum(test)){
    fail = use_factor[test]
    snd:::sys_abort_mtxMissingSelectedFactor(x = formatee,
                                             factor_missing = fail,
                                             name = formateeName)
  }
  #Return ####
  return(formatee)
}

#' @export
formatRI_key2mtx.sndkey_item = function(key, formater, formatee, formateeName){
  #Item checks if the formatee is described
  use_item = snd::grab_mtxItem(formatee)
  test = !(use_item %in% unique(formater$`@item`))
  if(sum(test)){
    fail = use_item[test]
    snd:::sys_abort_mtxMissingSelectedItem(x = formatee,
                                           item_missing = fail,
                                           name = formateeName)
  }
  #Return ####
  return(formatee)
}

#' @export
formatRI_key2mtx.sndkey_format = function(key, formater, formatee, formateeName){
  #Format checks if the formatees columns's datatype are described correctly
  #then formats it
  UseMethod(generic = "formatRI_key2mtx_sndkey_format", object = formater)
}

#' @export
formatRI_key2mtx_sndkey_format.snd_factor = function(key, formater, formatee, formateeName){
  #Check if contains @factor ####
  if(!("@factor" %in% colnames(formater))){
    snd:::sys_abort_mtxMissingSelectedKey(x = formater,
                                          keys_missing = "@factor",
                                          name = formateeName)
  }
  #Start using format ####
  factorName = snd::grab_mtxFactor(formatee)
  format = lapply(X = factorName,
                  FUN = function(X, df){
                    unique(unlist(dplyr::filter(.data = df, `@factor` == X)$`@format`)) %>%
                      snd:::classify(x = .,
                                     class = paste0("DT",
                                                    stringr::str_remove(string = .,
                                                                        pattern = "^#"))) %>%
                      return},
                  df = formater)
  factorData = lapply(X = factorName,
                      FUN = function(X, df){return(unname(unlist(dplyr::select(.data = df, {{X}}))))},
                      df = formatee)
  formated = mapply(FUN = snd:::formatRI_key2mtx_format_use,
                    format = format,
                    colItem = factorData,
                    colName = factorName,
                    mtxName = formateeName,
                    SIMPLIFY = FALSE, USE.NAMES = FALSE)

  #return the formated data back to formatee ###
  for(f in 1:length(factorName)){
    selName = factorName[[f]]
    selFormated = formated[[f]]
    formatee = dplyr::mutate(.data = formatee, "{selName}" := selFormated)
  }
  return(formatee)
}

#' @export
formatRI_key2mtx_sndkey_format.snd_item = function(key, formater, formatee, formateeName){
  #Check if contains @item ####
  if(!("@item" %in% colnames(formater))){
    snd:::sys_abort_mtxMissingSelectedKey(x = formater,
                                          keys_missing = "@item",
                                          name = formateeName)
  }
  #Start using format ####
  itemName = snd::grab_mtxItem(formatee)
  format = lapply(X = itemName,
                  FUN = function(X, df){
                    unique(unlist(dplyr::filter(.data = df, `@item` == X)$`@format`)) %>%
                      snd:::classify(x = .,
                                     class = paste0("DT",
                                                    stringr::str_remove(string = .,
                                                                        pattern = "^#"))) %>%
                      return},
                  df = formater)
  itemData = lapply(X = itemName,
                    FUN = function(X, df){return(unname(unlist(dplyr::select(.data = df, {{X}}))))},
                    df = formatee)
  formated = mapply(FUN = snd:::formatRI_key2mtx_format_use,
                    format = format,
                    colItem = itemData,
                    colName = itemName,
                    mtxName = formateeName,
                    SIMPLIFY = FALSE, USE.NAMES = FALSE)

  #return the formated data back to formatee ###
  for(f in 1:length(itemName)){
    selName = itemName[[f]]
    selFormated = formated[[f]]
    formatee = dplyr::mutate(.data = formatee, "{selName}" := selFormated)
  }
  return(formatee)
}

#' @export
formatRI_key2mtx.sndkey_label = function(key, formater, formatee, formateeName){
  #Label checks if the formatees value makes sense
  UseMethod(generic = "formatRI_key2mtx_sndkey_label", object = formater)
}

#' @export
formatRI_key2mtx_sndkey_label.snd_factor = function(key, formater, formatee, formateeName){
  #Check if contains @factor & @format####
  if(!("@factor" %in% colnames(formater))){
    snd:::sys_abort_mtxMissingSelectedKey(x = formater,
                                          keys_missing = "@factor",
                                          name = formateeName)
  }
  if(!("@format" %in% colnames(formater))){
    snd:::sys_abort_mtxMissingSelectedKey(x = formater,
                                          keys_missing = "@format",
                                          name = formateeName)
  }

  #Split and test ####
  factorName = snd:::grab_mtxFactor(formatee)
  format = lapply(X = factorName,
                  FUN = function(X, df){
                    unique(unlist(dplyr::filter(.data = df, `@factor` == X)$`@format`)) %>%
                      snd:::classify(x = .,
                                     class = paste0("DT",
                                                    stringr::str_remove(string = .,
                                                                        pattern = "^#"))) %>%
                      return},
                  df = formater)
  label  = lapply(X = factorName,
                  FUN = function(X, df){
                    return(unique(unlist(dplyr::filter(.data = df, `@factor` == X)$`@label`)))
                  }, df = formater)
  factorData = lapply(X = factorName,
                      FUN = function(X, df){
                        return(unique(unlist(dplyr::select(.data = df, {{X}}))))
                        }, df = formatee)
  supportedFormat = list(snd:::sys_format_support(with_abbr = T))
  test = mapply(FUN =
                  function(factorName, format, label, factorData, supportedFormat){
                    if("###" %in% label){
                      return(FALSE) #FALSE if everythings fine
                    } else {
                      defaultValue = unlist(dplyr::filter(.data = as.data.frame(supportedFormat),
                                                          full == unclass(stringr::str_remove(string = format, pattern = "^#")))$default)
                      label = unique(snd:::formatRI_key2mtx_format_use(format = format,
                                                                       colItem = c(label, defaultValue),
                                                                       force = TRUE))
                      return(as.logical(!sum(factorData %in% label)))
                    }
  },
  factorName = factorName, format = format, label = label, factorData = factorData, supportedFormat = supportedFormat,
  SIMPLIFY = TRUE, USE.NAMES = FALSE)
  if(sum(test)){
    snd:::sys_abort_mtxKeyLabelIncorrectlyDescribeData(x = formatee,
                                                       name = formateeName,
                                                       failed_columns = factorName[test])
  }
  #Return if OK
  return(formatee)
}

#' @export
formatRI_key2mtx_sndkey_label.snd_item = function(key, formater, formatee, formateeName){
  #Check if contains @factor & @format####
  if(!("@item" %in% colnames(formater))){
    snd:::sys_abort_mtxMissingSelectedKey(x = formater,
                                          keys_missing = "@item",
                                          name = formateeName)
  }
  if(!("@format" %in% colnames(formater))){
    snd:::sys_abort_mtxMissingSelectedKey(x = formater,
                                          keys_missing = "@format",
                                          name = formateeName)
  }

  #Split and test ####
  itemName = snd:::grab_mtxItem(formatee)
  format = lapply(X = itemName,
                  FUN = function(X, df){
                    unique(unlist(dplyr::filter(.data = df, `@item` == X)$`@format`)) %>%
                    snd:::classify(x = .,
                                   class = paste0("DT",
                                                  stringr::str_remove(string = ., pattern = "^#"))) %>%
                    return}, df = formater)
  label  = lapply(X = itemName,
                  FUN = function(X, df){return(unique(unlist(dplyr::filter(.data = df, `@item` == X)$`@label`)))},
                  df = formater)
  itemData = lapply(X = itemName,
                    FUN = function(X, df){return(unique(unlist(dplyr::select(.data = df, {{X}}))))},
                    df = formatee)
  supportedFormat = list(snd:::sys_format_support(with_abbr = T))

  test = mapply(FUN =
                  function(itemName, format, label, itemData, supportedFormat){
                    if("###" %in% label){
                      return(FALSE) #FALSE if everythings fine
                    } else {
                      defaultValue = unlist(dplyr::filter(.data = as.data.frame(supportedFormat),
                                                          full == unclass(stringr::str_remove(string = format, pattern = "^#")))$default)
                      label = unique(snd:::formatRI_key2mtx_format_use(format = format,
                                                                       colItem = c(label, defaultValue),
                                                                       force = TRUE))
                      return(as.logical(!sum(itemData %in% label)))
                    }
                  },
                itemName = itemName, format = format, label = label, itemData = itemData, supportedFormat = supportedFormat,
                SIMPLIFY = TRUE, USE.NAMES = FALSE)
  if(sum(test)){
    snd:::sys_abort_mtxKeyLabelIncorrectlyDescribeData(x = formatee,
                                                       name = formateeName,
                                                       failed_columns = itemName[test])
  }
  #Return if OK
  return(formatee)
}
