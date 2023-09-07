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
#' @keywords internal
#' @rdname formatRI_key2mtx
formatRI_key2mtx = function(key, formater, formatee, formateeName){
  #Check ####
  if(rlang::is_missing(key)){snd:::sys_abort_NoArg(key)}
  if(rlang::is_missing(formater)){snd:::sys_abort_NoArg(formater)}
  if(rlang::is_missing(formatee)){snd:::sys_abort_NoArg(formatee)}
  if(length(key) != 1){snd:::sys_abort_WrongLength(x = key, length = 1L)}
  valid_key = key %in% snd::grab_mtxKey(formater)
  if(!valid_key){snd:::sys_abort_mtxMissingKey(x = mtx, keys_missing = key, name = mtxName)}

  #Call other functions to format using the keys ####
  key = stringr::str_sub(string = key, start = 2L, end = -1L)
  class(key) = paste0("sndkey_", key)
  UseMethod(generic = "formatRI_key2mtx", object = key)
}

#' @export
#' @rdname formatRI_key2mtx
formatRI_key2mtx.sndkey_factor = function(key, formater, formatee, formateeName){
  #Check if all the factors in formatee are listed in formater ####
  use_factor = unique(snd:::grab_mtxFactor(dataframe = formatee))
  ava_factor = unique(formater$`@factor`)
  if(sum(!(use_factor %in% ava_factor))){
    failed_factor = use_factor[!ava_factor]
    snd:::sys_abort_mtxMissingSelectedFactor(x = formatee,
                                             factor_missing = failed_factor,
                                             name = formateeName)
  }
  #Return ####
  return(formatee)
}

#' @export
#' @rdname formatRI_key2mtx
formatRI_key2mtx.sndkey_item = function(key, formater, formatee, formateeName){
  #Check if all the items in formatee are listed in formater ####
  use_item = unique(snd:::grab_mtxItem(dataframe = formatee))
  ava_item = unique(formater$`@item`)
  if(sum(!(use_item %in% ava_item))){
    failed_item = use_item[!ava_item]
    snd:::sys_abort_mtxMissingSelectedItem(x = formatee,
                                           item_missing = failed_item,
                                           name = formateeName)
  }
  #Return ####
  return(formatee)
}

#' @export
#' @rdname formatRI_key2mtx
formatRI_key2mtx.sndkey_type = function(key, formater, formatee, formateeName){
  #Return ####
  return(formatee)
}

#' @export
#' @rdname formatRI_key2mtx
formatRI_key2mtx.sndkey_format = function(key, formater, formatee, formateeName){
  if("snd_factor" %in% class(formater)){
    use_factor = snd:::grab_mtxFactor(formatee)
    for(i in use_factor){
      selected_format = dplyr::filter(formater, `@factor` == i)
      format = selected_format %>%
        dplyr::mutate(prefix = snd::grab_keyPrefix(mtx = ., key = "@format"),
                      head = snd::grab_keyHead(mtx = ., key = "@format"),
                      full = ifelse(is.na(prefix), head, paste0(prefix, head)))
      unique_format = unique(x = format$full)
      class(unique_format) = paste0("DT", unique(x = format$head))
      colItem = unlist(dplyr::select(.data = formatee, {{i}}))
      colItem = snd:::formatRI_key2mtx_format_use(format = unique_format,
                                                  colItem = colItem,
                                                  colName = i,
                                                  mtxName = formateeName)
      formatee = dplyr::mutate(.data = formatee, "{i}" := colItem)
    }
    return(invisible(formatee))
  }
  if("snd_item" %in% class(formater)){
    use_item = snd:::grab_mtxItem(formatee)
    for(i in use_item){
      selected_format = dplyr::filter(formater, `@item` == i)
      format = selected_format %>%
        dplyr::mutate(prefix = snd::grab_keyPrefix(mtx = ., key = "@format"),
                      head = snd::grab_keyHead(mtx = ., key = "@format"),
                      full = ifelse(is.na(prefix), head, paste0(prefix, head)))
      unique_format = unique(x = format$full)
      class(unique_format) = paste0("DT", unique(x = format$head))
      colItem = unlist(dplyr::select(.data = formatee, {{i}}))
      colItem = snd:::formatRI_key2mtx_format_use(format = unique_format,
                                                  colItem = colItem,
                                                  colName = i,
                                                  mtxName = formateeName)
      formatee = dplyr::mutate(.data = formatee, "{i}" := colItem)
    }
    return(invisible(formatee))
  }
}
