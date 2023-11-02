#' @rdname formatRI_key2mtx
#' @keywords internal
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
  seperated_item = lapply(X = ava_item,
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
  use_format = lapply(X = index,
                      FUN = function(X, seperated_item){
                        seperated_item[[X]]$`@format`  %>%
                          unlist(use.names = FALSE) %>%
                          unique %>%
                          snd:::classify(x = .,
                                         class = paste0("DT",
                                                        stringr::str_remove(string = .,
                                                                            pattern = "^#"))) %>%
                          return},
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

#' @rdname formatWO_key2mtx
#' @keywords internal
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
