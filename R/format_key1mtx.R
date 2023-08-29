#' System tools: Checking and Formatting the keys during read-in.
#'
#' @description
#' In SND, a `key` is a column within the matrix that can be used to check and modify itself, it's matrix and other matrixes. This function checks and modifies it's own matrix only, using a key. To modify other matrixes, check the function `snd:::format_key2mtx`. To enhance security, only 1 key will be accepted.
#'
#' @param key The key in `character`. Only 1 key will be accepted.
#' @param mtx The matrix in `data.frame`. The matrix must contain the key.
#' @param mtxName The name of the matrix in character.
#'
#' @return A matrix, checked (or/and modified) by the key.
#' @keywords internal
#' @rdname format_key
format_key = function(key, mtx, mtxName){
  #Check ####
  if(rlang::is_missing(key)){snd:::sys_abort_NoArg(key)}
  if(rlang::is_missing(mtx)){snd:::sys_abort_NoArg(mtx)}
  if(length(key) != 1){snd:::sys_abort_WrongLength(x = key, length = 1L)}
  valid_key = key %in% snd::grab_mtxKey(mtx)
  if(!valid_key){
    snd:::sys_abort_mtxMissingKey(x = mtx, keys_missing = key, name = mtxName)
  }
  #Call other functions to format using the keys ####
  key = stringr::str_sub(string = key, start = 2L, end = -1L)
  class(key) = paste0("sndkey_", key)
  UseMethod(generic = "format_key", object = key)
}

#' @export
#' @rdname format_key
format_key.sndkey_type = function(key, mtx, mtxName){
  #Check if @type is of the following classes.
  #If not, type it as "error"
  keyType = mtx$`@type`
  keyType_CorrectTest = keyType %in% c("data", "calc", "stat")
  if(sum(!keyType_CorrectTest)){
    mtx = dplyr::mutate(.data = mtx,
                        `@type` = ifelse(!keyType_CorrectTest, "error", `@type`))
    if(rlang::is_missing(mtxName)){
      snd:::sys_warn(message = c("!" = "Include incorrect {.col @type} in {.arg {arg}}",
                                 "i" = "All incorrect types are changed to {.code error}"),
                     x = mtx)
    } else {
      snd:::sys_warn(message = c("!" = "Include incorrect {.col @type} in {.mtx {mtxName}}",
                                 "i" = "All incorrect types are changed to {.code error}"),
                     x = mtx, mtxName = mtxName)
    }
  }
  return(invisible(mtx))
}

#' @export
#' @rdname format_key
format_key.sndkey_item = function(key, mtx, mtxName){
  #Check if @item is filled or not
  keyItem = mtx$`@item`
  if(sum(is.na(keyItem) | keyItem == "#NA")){
    snd:::sys_abort_mtxColNotAllFilled(x = mtx, name = mtxName, columns = "@item")
  }
  #Check if @item is a factor or not
  test = stringr::str_sub(string = keyItem, start = 1L, end = 1L)
  if(sum(test == "#")){
    snd:::sys_abort_mtxColContainFactor(x = mtx, name = mtxName, columns = "@item")
  }
  return(invisible(mtx))
}

#' @export
#' @rdname format_key
format_key.sndkey_datatype = function(key, mtx, mtxName){
  UseMethod(generic = "format_key.sndkey_datatype", object = mtx)
}

#' @export
#' @rdname format_key
format_key.sndkey_datatype.snd_factor = function(key, mtx, mtxName){
  #Check 0, mtx contains @factor ####
  if(!("@factor" %in% colnames(mtx))){snd:::sys_abort_mtxMissingKey(x = mtx,
                                                                    keys_missing = "@factor",
                                                                    name = mtxName)}
  #Try to format everything ####
  sys_datatype = snd:::sys_datatype_support(with_abbr = TRUE)
  formated_dt = tibble::tibble(element = mtx$`@factor`,
                               DT = mtx$`@datatype`,
                               head = snd:::grab_keyHead(mtx = mtx, key = "@datatype"),
                               tail = snd:::grab_keyTail(mtx = mtx, key = "@datatype")) %>%
    dplyr::mutate(head = ifelse(is.na(head), "factor", head),
                  match_abbr = match(x = head, table = sys_datatype$abbr),
                  match_abbr = ifelse(is.na(match_abbr), 0, match_abbr),

                  match_alias = match(x = head, table = sys_datatype$alias),
                  match_alias = ifelse(is.na(match_alias), 0, match_alias),

                  match_full = match(x = head, table = sys_datatype$full),
                  match_full = ifelse(is.na(match_full), 0, match_full),
                  match = match_abbr + match_alias + match_full,
                  fullname = sys_datatype$full[match],
                  fullname_withtail = ifelse(is.na(tail), fullname, paste0(fullname, "_", tail)))
  #Check 1, No match ####
  if(sum(formated_dt$match == 0)){
    unsupported = unique(dplyr::filter(.data = formated_dt, match == 0)$DT)
    snd:::sys_abort_mtxKeyDatatypeUnsupported(x = mtx,
                                              name = mtxName,
                                              unsupport_datatype = unsupported)
  }
  #Check 2, Full name inconsistent with @factor ####
  unique_element = unique(formated_dt$element)
  mtx_seperated = lapply(X = unique_element,
                         FUN = function(X, df){return(dplyr::filter(.data = df, element == X))},
                         df = formated_dt)
  test = unlist(lapply(X = mtx_seperated, FUN = function(X){return(length(unique(X$fullname)) == 1)}))
  if(sum(!test)){
    failed_groups = unique_element[!test]
    snd:::sys_abort_mtxColGrpItemsNotSame(x = mtx,
                                          name = mtxName,
                                          columns = "@datatype",
                                          group_by = "@factor",
                                          failed_groups = failed_groups)
  }
  #Return ####
  mtx = dplyr::mutate(.data = mtx, `@datatype` := formated_dt$fullname_withtail)
  return(invisible(mtx))
}

#' @export
#' @rdname format_key
format_key.sndkey_datatype.snd_item = function(key, mtx, mtxName){
  #Check 0, mtx contains @item ####
  if(!("@item" %in% colnames(mtx))){snd:::sys_abort_mtxMissingKey(x = mtx,
                                                                  keys_missing = "@item",
                                                                  name = mtxName)}
  #Try to format everything ####
  sys_datatype = snd:::sys_datatype_support(with_abbr = TRUE)
  formated_dt = tibble::tibble(element = mtx$`@item`,
                               DT = mtx$`@datatype`,
                               head = snd:::grab_keyHead(mtx = mtx, key = "@datatype"),
                               tail = snd:::grab_keyTail(mtx = mtx, key = "@datatype")) %>%
    dplyr::mutate(head = ifelse(is.na(head), "factor", head),
                  match_abbr = match(x = head, table = sys_datatype$abbr),
                  match_abbr = ifelse(is.na(match_abbr), 0, match_abbr),

                  match_alias = match(x = head, table = sys_datatype$alias),
                  match_alias = ifelse(is.na(match_alias), 0, match_alias),

                  match_full = match(x = head, table = sys_datatype$full),
                  match_full = ifelse(is.na(match_full), 0, match_full),
                  match = match_abbr + match_alias + match_full,
                  fullname = sys_datatype$full[match],
                  fullname_withtail = ifelse(is.na(tail), fullname, paste0(fullname, "_", tail)))
  #Check 1, No match ####
  if(sum(formated_dt$match == 0)){
    unsupported = unique(dplyr::filter(.data = formated_dt, match == 0)$DT)
    snd:::sys_abort_mtxKeyDatatypeUnsupported(x = mtx,
                                              name = mtxName,
                                              unsupport_datatype = unsupported)
  }
  #Check 2, Full name inconsistent with @factor ####
  unique_element = unique(formated_dt$element)
  mtx_seperated = lapply(X = unique_element,
                         FUN = function(X, df){return(dplyr::filter(.data = df, element == X))},
                         df = formated_dt)
  test = unlist(lapply(X = mtx_seperated, FUN = function(X){return(length(unique(X$fullname)) == 1)}))
  if(sum(!test)){
    failed_groups = unique_element[!test]
    snd:::sys_abort_mtxColGrpItemsNotSame(x = mtx,
                                          name = mtxName,
                                          columns = "@datatype",
                                          group_by = "@item",
                                          failed_groups = failed_groups)
  }
  #Return ####
  mtx = dplyr::mutate(.data = mtx, `@datatype` := formated_dt$fullname_withtail)
  return(invisible(mtx))
}

#' @export
#' @rdname format_key
format_key.sndkey_factor = function(key, mtx, mtxName){
  #Check if @factor is all filled or not
  keyFactor = mtx$`@factor`
  if(sum(is.na(keyFactor) | keyFactor == "#NA")){
    snd:::sys_abort_mtxColNotAllFilled(x = mtx, name = mtxName, columns = "@factor")
  }
  #Check if @item is a factor or not
  test = stringr::str_sub(string = keyFactor, start = 1L, end = 1L)
  if(sum(test != "#")){
    snd:::sys_abort_mtxColContainFactorFactor(x = mtx, name = mtxName, columns = "@factor")
  }
  return(invisible(mtx))
}

#' @export
#' @rdname format_key
format_key.sndkey_label = function(key, mtx, mtxName){
  UseMethod(generic = "format_key.sndkey_label", object = mtx)
}

#' @export
#' @rdname format_key
format_key.sndkey_label.snd_factor = function(key, mtx, mtxName){
  colnames = colnames(mtx)
  #Checkpoint 1: Check if the matrix contains @factor/ @item
  if(!("@factor" %in% colnames)){snd:::sys_abort_mtxMissingKey(x = mtx, keys_missing = "@factor", name = mtxName)}
  #Checkpoint 2: separate the matrix by @factor, check if @label has repetitive items
  unique_factor = unique(mtx$`@factor`)
  seperated_matrix = lapply(X = unique_factor,
                            FUN = function(X, mtx){return(dplyr::filter(.data = mtx, `@factor` == X))},
                            mtx = mtx)
  test_duplicate = unlist(lapply(X = seperated_matrix, FUN = function(X){duplicated(X$`@label`) %>% sum() %>% as.logical() %>% return()}))

  if(sum(test_duplicate)){
    failed_groups = unique_factor[test_duplicate]
    snd:::sys_abort_mtxColGrpItemsNotUnique(x = mtx, name = mtxName, columns = "@label", group_by = "@factor", failed_groups = failed_groups)
  }
  #Checkpoint 3: check if ### is included with other labels
  test_hash = unlist(lapply(X = seperated_matrix, FUN = function(X){return("###" %in% X$`@label`)}))
  test_nrow = unlist(lapply(X = seperated_matrix, FUN = function(X){return(nrow(X) > 1)}))
  test_binded = test_hash & test_nrow

  if(sum(test_binded)){
    failed_groups = unique_factor[test_binded]
    snd:::sys_abort_mtxColGrpItemsNotExclusive(x = mtx, name = mtxName,
                                               columns = "@label",
                                               group_by = "@factor",
                                               failed_groups = failed_groups,
                                               exclusive_item = "###")
  }
  #Return ####
  return(invisible(mtx))
}

#' @export
#' @rdname format_key
format_key.sndkey_label.snd_item = function(key, mtx, mtxName){
  colnames = colnames(mtx)
  #Checkpoint 1: Check if the matrix contains @factor/ @item
  if(!("@item" %in% colname)){snd:::sys_abort_mtxMissingKey(x = mtx, keys_missing = "@item", name = mtxName)}
  #Checkpoint 2: separate the matrix by @factor, check if @label has repetitive items
  unique_item = unique(mtx$`@item`)
  seperated_matrix = lapply(X = unique_item,
                            FUN = function(X, mtx){return(dplyr::filter(.data = mtx, `@item` == X))},
                            mtx = mtx)
  test_duplicate = unlist(lapply(X = seperated_matrix, FUN = function(X){duplicated(X$`@label`) %>% sum() %>% as.logical() %>% return()}))

  if(sum(test_duplicate)){
    failed_groups = unique_item[test_duplicate]
    snd:::sys_abort_mtxColGrpItemsNotUnique(x = mtx, name = mtxName,
                                            columns = "@label",
                                            group_by = "@item",
                                            failed_groups = failed_groups)
  }
  #Checkpoint 3: check if ### is included with other labels
  test_hash = unlist(lapply(X = seperated_matrix, FUN = function(X){return("###" %in% X$`@label`)}))
  test_nrow = unlist(lapply(X = seperated_matrix, FUN = function(X){return(nrow(X) > 1)}))
  test_binded = test_hash & test_nrow

  if(sum(test_binded)){
    failed_groups = unique_factor[test_binded]
    snd:::sys_abort_mtxColGrpItemsNotExclusive(x = mtx, name = mtxName,
                                               columns = "@label",
                                               group_by = "@item",
                                               failed_groups = failed_groups,
                                               exclusive_item = "###")
  }
  #Return ####
  return(invisible(mtx))
}
