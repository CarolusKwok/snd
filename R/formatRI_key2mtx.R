#' System tools: Checking and Formatting other matrixs using keys during read-in.
#'
#' @description
#' In SND, a `key` is a column within the matrix that can be used to check and modify itself, it's matrix and other matrices. This function uses a key within `formater`, to checks and modifies another matrix `formatee`, using a key. To modify it's own matrix, check the function `snd:::formatRI_key`. To enhance security, only 1 key will be accepted.
#'
#' @param key The key in `character`. Only 1 key will be accepted.
#' @param formater The matrix in `data.frame` to format the formatee. The matrix must contain the key.
#' @param formatee The matrix in `data.frame` to be formatted using formater's keys.
#' @param formaterName
#' @param formateeName The name of the formatee in `character`.
#'
#' @return The formatee matrix, checked (and formatted) by formater matrix.
#' @export
formatRI_key2mtx = function(key, formater, formatee, formaterName, formateeName){
  #Check ####
  if(rlang::is_missing(key)){snd:::sys_abort_NoArg(key)}
  if(rlang::is_missing(formater)){snd:::sys_abort_NoArg(formater)}
  if(rlang::is_missing(formatee)){snd:::sys_abort_NoArg(formatee)}
  if(rlang::is_missing(formaterName)){snd:::sys_abort_NoArg(formaterName)}
  if(rlang::is_missing(formateeName)){snd:::sys_abort_NoArg(formateeName)}
  if(length(key) != 1){snd:::sys_abort_WrongLength(x = key, length = 1L)}
  valid_key = key %in% snd::grab_mtxKey(formater)
  if(!valid_key){snd:::sys_abort_mtxMissingKey(x = mtx, keys_missing = key, name = mtxName)}

  #Call other functions to format using the keys ####
  key = snd:::classify_key(stringr::str_remove(string = key, pattern = "^@"))
  UseMethod(generic = "formatRI_key2mtx", object = key)
}

#' @export
formatRI_key2mtx.sndkey_factor = function(key, formater, formatee, formaterName, formateeName){
  ava_factor = formater$`@factor`
  #1. Check if @factor is all filled ####
  test = sum(is.na(ava_factor) | ava_factor == "#NA")
  if(test){
    snd:::sys_abort(message = c("x" = "{.col @factor} not filled in {.mtx {formaterName}}",
                                "i" = "{.col @factor} must be all filled to function properly",
                                "i" = "{.code #NA} is not accepted"),
                    formaterName = formaterName)
  }

  #2. Check if @factor contains items ####
  test = !stringr::str_detect(string = ava_factor, pattern = "^#")
  if(sum(test)){
    snd:::sys_abort(message = c("x" = "{.mtx {formaterName}} contain items in {.col @factor}",
                                "!" = "Items include:",
                                "i" = snd:::sys_message_code(code = ava_factor[test])),
                    formaterName = formaterName)
  }

  #3. Checks if the formatee described ####
  use_factor = snd::grab_mtxFactor(formatee)
  test = !(use_factor %in% unique(formater$`@factor`))
  if(sum(test)){
    snd:::sys_abort(message = c("x" = "Missing factors in {.mtx {formaterName}} {.col @factor}",
                                "i" = "Factors missing:",
                                "i" = snd:::sys_message_columns(columns = use_factor[test])),
                    formaterName = formaterName)
  }
  #Return ####
  return(list(formater = formater,
              formatee = formatee))
}

#' @export
formatRI_key2mtx.sndkey_item = function(key, formater, formatee, formaterName, formateeName){
  ava_item = mtx$`@item`
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
                                "i" = snd:::sys_message_code(code = ava_item[test])),
                    formaterName = formaterName)
  }
  #3. Checks if the formatee described ####
  use_item = snd::grab_mtxItem(formatee)
  test = !(use_item %in% unique(formater$`@item`))
  if(sum(test)){
    snd:::sys_abort(message = c("x" = "Missing items in {.mtx {formaterName}} {.col @item}",
                                "i" = "Items missing:",
                                "i" = snd:::sys_message_columns(columns = use_item[test])),
                    formaterName = formaterName)
  }
  #Return ####
  return(list(formater = formater,
              formatee = formatee))
}

#' @export
formatRI_key2mtx.sndkey_type = function(key, formater, formatee, formaterName, formateeName){
  #Doesn't really interact with other matrices
  #1. Check if @type if of the following classes ####
  return(list(formater = snd:::formatRI_key.sndkey_type(key = key,
                                                        mtx = formater,
                                                        mtxName = formaterName),
              formatee = formatee))
}

#' @export
formatRI_key2mtx.sndkey_format = function(key, formater, formatee, formaterName, formateeName){
  #1. Check if the formats are acceptable ####
  sys_format = snd:::sys_format_support(with_abbr = TRUE)
  format = data.frame(format = formater$`@format`,
                      prefix = snd::grab_keyPrefix(mtx = formater, key = "@format"),
                      head = snd:::grab_keyHead(mtx = formater, key = "@format"),
                      tail = snd::grab_keyTail(mtx = formater, key = "@format")) %>%
    dplyr::mutate(match_abbr = match(x = head, table = sys_format$abbr, nomatch = 0L),
                  match_alias = match(x = head, table = sys_format$alias, nomatch = 0L),
                  match_full = match(x = head, table = sys_format$full, nomatch = 0L),
                  match = match_abbr + match_alias + match_full,

                  full = ifelse(match == 0, NA, sys_format$full[match]),
                  fullname = ifelse(is.na(prefix), full, paste0(prefix, full)),
                  fullname = ifelse(is.na(tail), fullname, paste0(fullname, "_", tail)))

  if(sum(formated_dt$match == 0)){
    snd:::sys_abort(message = c("x" = "Unsupported format in {.mtx {formaterName}}",
                                "!" = "Unsupported format include",
                                "!" = snd:::sys_message_code(code = unique(dplyr::filter(.data = format, match == 0)$format)),
                                "i" = "Please specify supported format in {.col @format}",
                                "i" = "Supported format include following:",
                                "i" = snd:::sys_message_code(code = snd:::sys_format_support(with_abbr = FALSE))),
                    formaterName = formaterName)
  }

  #2. Apply the formatted format into formater ####
  formater = dplyr::mutate(.data = formater,
                           `@format` = format$fullname)

  #3. Use Methods ####
  return(snd:::formatRI_key2mtx_sndkey_format(key = key,
                                              formater = formater,
                                              formatee = formatee,
                                              formaterName = formaterName,
                                              formateeName = formateeName))
}

#' @rdname formatRI_key2mtx
#' @keywords Internal
formatRI_key2mtx_sndkey_format = function(key, formater, formatee, formaterName, formateeName){
  UseMethod(generic = "formatRI_key2mtx_sndkey_format", object = formater)
}

#' @export
formatRI_key2mtx_sndkey_format.snd_factor = function(key, formater, formatee, formaterName, formateeName){
  #1. Check consistent @format across @factor####
  ava_factor = unique(formater$`@factor`)
  seperated_factor = lapply(X = ava_factor,
                            FUN = function(X, formater){formater = duplyr::filter(formater, `@factor` == X)})
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
  for(f in 1:length(use_factor)){
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
                          FUN = function(X, formater){
                            formater = duplyr::filter(formater, `@item` == X)
                            })
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
  for(f in 1:length(use_item)){
    selName = use_item[[f]]
    selFormated = formated[[f]]
    formatee = dplyr::mutate(.data = formatee, "{selName}" := selFormated)
  }

  return(list(formater = formater,
              formatee = formatee))
}

#' @export
formatRI_key2mtx.sndkey_label = function(key, formater, formatee, formaterName, formateeName){
  #Use methods####
  return(snd:::formatRI_key2mtx_sndkey_label(key = key,
                                             formater = formater,
                                             formatee = formatee,
                                             formaterName = formaterName,
                                             formateeName = formateeName))
}

#' @rdname formatRI_key2mtx
#' @keywords Internal
formatRI_key2mtx_sndkey_label = function(key, formater, formatee, formaterName, formateeName){
  UseMethod(generic = "formatRI_key2mtx_sndkey_label", object = formater)
}

#' @export
formatRI_key2mtx_sndkey_label.snd_factor = function(key, formater, formatee, formaterName, formateeName){
  #1. separate formater by @factor, check if @label has repetitive items ####
  ava_factor = unique(formater$`@factor`)
  seperated_factor = lapply(X = ava_factor,
                            FUN = function(X, formater){return(dplyr::filter(.data = formater, `@factor` == X))},
                            formater = formater)
  test = sapply(X = seperated_factor, FUN = function(X){return(sum(duplicated(X$`@label`)))},
                simplify = TRUE, USE.NAMES = FALSE)
  if(sum(test)){
    snd:::sys_abort(message = c("x" = "Non-unique items in {.mtx {formaterName}} {.col @label}, when grouped",
                                "i" = "Grouped by: {.col @factor}",
                                "i" = "Abnormal group:",
                                "i" = snd:::sys_message_columns(columns = ava_factor[test])),
                    formaterName = formaterName)
  }

  #2. check if ### is included with other labels ####
  test_hash = sapply(X = seperated_factor, FUN = function(X){return("###" %in% X$`@label`)}, simplify = TRUE, USE.NAMES = FALSE)
  test_nrow = sapply(X = seperated_factor, FUN = function(X){return(nrow(X) > 1)}, simplify = TRUE, USE.NAMES = FALSE)
  test = test_hash & test_nrow

  if(sum(test)){
    snd:::sys_abort(message = c("x" = "Exclusive items in {.mtx {formaterName}} {.col @label} columns, when grouped",
                                "i" = "Grouped by: {.col @factor}",
                                "i" = "Abnormal group:",
                                "i" = snd:::sys_message_columns(columns = unique_factor[test]),
                                "i" = "Exclusive item: {.code ###}"),
                    formaterName = formaterName)
  }

  #3. Turn #NA into actually NA ####
  formater = dplyr::mutate(.data = formater,
                           `@label` = ifelse(`@label` == "#NA", NA, `@label`))

  #4. Split and test ####
  use_factor = snd:::grab_mtxFactor(formatee)
  index = match(x = use_factor, table = ava_factor, nomatch = 0L)
  use_format = lapply(X = index,
                      FUN = function(X, seperated_factor){
                        seperated_factor[[X]]$`@format` %>%
                          unlist(use.names = FALSE) %>%
                          unique %>%
                          snd:::classify(x = .,
                                         class = paste0("DT",
                                                        stringr::str_remove(string = .,
                                                                            pattern= "^#"))) %>%
                          return},
                      seperated_factor = seperated_factor)
  use_label = lapply(X = index,
                     FUN = function(X, seperated_factor){
                       seperated_factor[[X]]$`@label` %>%
                         unlist(use.names = FALSE) %>%
                         unique %>%
                         return},
                     seperated_factor = seperated_factor)
  ava_data = lapply(X = use_factor,
                    FUN = function(X, formatee){
                      dplyr::select(.data = formatee, {{X}}) %>%
                        unlist(use.names = FALSE) %>%
                        unique %>%
                        return},
                    formatee = formatee)
  supportedFormat = list(snd:::sys_format_support(with_abbr = TRUE))
  test = mapply(FUN =
                  function(use_factor, use_format, use_label, ava_data, supportedFormat){
                    if("###" %in% label){
                      return(FALSE) #FALSE if everythings fine
                    } else {
                      defaultValue = dplyr::filter(.data = as.data.frame(supportedFormat),
                                                   full == unclass(stringr::str_remove(string = use_format, pattern = "^#")))$default[[1]]
                      use_label = unique(snd:::formatRI_key2mtx_format_use(format = use_format,
                                                                           colItem = c(use_label, defaultValue),
                                                                           force = TRUE))
                      return(!sum(ava_data %in% use_label))
                    }
  },
  use_factor = use_factor, use_format = use_format, use_label = use_label,
  ava_data = ava_data, supportedFormat = supportedFormat,
  SIMPLIFY = TRUE, USE.NAMES = FALSE)
  if(sum(test)){
    snd:::sys_abort(message = c("x" = "Describing columns incorrectly in {.mtx {formateeName}} using {.col @label}",
                                "!" = "Incorrectly-described columns:",
                                "!" = snd:::sys_message_columns(columns = use_factor[test])),
                    formateeName = formateeName)
  }
  #Return if OK
  return(list(formater = formater,
              formatee = formatee))
}

#' @export
formatRI_key2mtx_sndkey_label.snd_item = function(key, formater, formatee, formaterName, formateeName){
  #1. separate the matrix by @item, check if @label has repetitive items ####
  ava_item = unique(formater$`@item`)
  seperated_item = lapply(X = unique_item,
                          FUN = function(X, formater){return(dplyr::filter(.data = formater, `@item` == X))},
                          formater = formater)
  test = sapply(X = seperated_item, FUN = function(X){return(sum(duplicated(X$`@label`)))},
                simplify = TRUE, USE.NAMES = FALSE)
  if(sum(test)){
    snd:::sys_abort(message = c("x" = "Non-unique items in {.mtx {formaterName}} {.col @label}, when grouped",
                                "i" = "Grouped by: {.col @item}",
                                "i" = "Abnormal group:",
                                "i" = snd:::sys_message_columns(columns = ava_item[test])),
                    formaterName = formaterName)
  }

  #2. check if ### is included with other labels ####
  test_hash = sapply(X = seperated_item, FUN = function(X){return("###" %in% X$`@label`)}, simplify = TRUE, USE.NAMES = FALSE)
  test_nrow = sapply(X = seperated_item, FUN = function(X){return(nrow(X) > 1)}, simplify = TRUE, USE.NAMES = FALSE)
  test = test_hash & test_nrow

  if(sum(test)){
    snd:::sys_abort(message = c("x" = "Exclusive items in {.mtx {formaterName}} {.col @label}, when grouped",
                                "i" = "Grouped by: {.col @item}",
                                "i" = "Abnormal group:",
                                "i" = snd:::sys_message_columns(ava_item[test]),
                                "i" = "Exclusive item include: {.code ###}"),
                    formaterName = formaterName)
  }

  #3. Turn #NA into actually NA ####
  formater = dplyr::mutate(.data = formater,
                           `@label` = ifelse(`@label` == "#NA", NA, `@label`))

  #4. Split and test ####
  use_item = snd:::grab_mtxItem(formatee)
  index = match(x = use_item, table = ava_item, nomatch = 0L)
  use_format = lapply(X = use_item,
                      FUN = function(X, seperated_item){
                        seperated_item[[X]]$`@format`  %>%
                          unlist(use.names = FALSE) %>%
                          unique %>%
                          snd:::classify(x = .,
                                         class = paste0("DT",
                                                        stringr::str_remove(string = .,
                                                                            pattern = "^#"))) %>%
                          return
                      },
                      seperated_item = seperated_item)
  use_label = lapply(X = index,
                     FUN = function(X, seperated_item){
                       seperated_item[[X]]$`@label` %>%
                         unlist(use.names = FALSE) %>%
                         unique %>%
                         return},
                     seperated_item = seperated_item)
  ava_data = lapply(X = use_item,
                    FUN = function(X, formatee){
                      dplyr::select(.data = formatee, {{X}}) %>%
                        unlist(use.names = FALSE) %>%
                        unique %>%
                        return},
                    formatee = formatee)
  supportedFormat = list(snd:::sys_format_support(with_abbr = TRUE))

  test = mapply(FUN =
                  function(use_item, use_format, use_label, ava_data, supportedFormat){
                    if("###" %in% use_label){
                      return(FALSE) #FALSE if everythings fine
                    } else {
                      defaultValue = dplyr::filter(.data = as.data.frame(supportedFormat),
                                                   full == unclass(stringr::str_remove(string = use_format, pattern = "^#")))$default[[1]]
                      use_label = unique(snd:::formatRI_key2mtx_format_use(format = use_format,
                                                                           colItem = c(use_label, defaultValue),
                                                                           force = TRUE))
                      return(!sum(ava_data %in% use_label))
                    }
                  },
                use_item = use_item, use_format = use_format, use_label = use_label,
                ava_data = ava_data, supportedFormat = supportedFormat,
                SIMPLIFY = TRUE, USE.NAMES = FALSE)
  if(sum(test)){
    snd:::sys_abort(message = c("x" = "Describing columns incorrectly in {.mtx {formateeName}} using {.col @label}",
                                "!" = "Incorrectly-described columns:",
                                "!" = snd:::sys_message_columns(columns = use_item[test])),
                    formateeName = formateeName)
  }
  #Return if OK
  return(list(formater = formater,
              formatee = formatee))
}
