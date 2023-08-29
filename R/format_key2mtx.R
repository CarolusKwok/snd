#' System tools: Checking and Formatting other matrixs using keys during read-in.
#'
#' @description
#' In SND, a `key` is a column within the matrix that can be used to check and modify itself, it's matrix and other matrices. This function uses a key within `formater`, to checks and modifies another matrix `formatee`, using a key. To modify it's own matrix, check the function `snd:::format_key`. To enhance security, only 1 key will be accepted.
#'
#' @param key The key in `character`. Only 1 key will be accepted.
#' @param formater The matrix in `data.frame` to format the formatee. The matrix must contain the key.
#' @param formatee The matrix in `data.frame` to be formatted using formater's keys.
#' @param formateeName The name of the formatee in `character`.
#'
#' @return The formatee matrix, checked (and formatted) by formater matrix.
#' @keywords internal
#' @rdname format_key2mtx
format_key2mtx = function(key, formater, formatee, formateeName){
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
  UseMethod(generic = "format_key2mtx", object = key)
}

#' @export
#' @rdname format_key2mtx
format_key2mtx.sndkey_factor = function(key, formater, formatee, formateeName){
  return(formatee)
}

#' @export
#' @rdname format_key2mtx
format_key2mtx.sndkey_item = function(key, formater, formatee, formateeName){
  return(formatee)
}

#' @export
#' @rdname format_key2mtx
format_key2mtx.sndkey_type = function(key, formater, formatee, formateeName){
  return(formatee)
}

#' @export
#' @rdname format_key2mtx
format_key2mtx.sndkey_label = function(key, formater, formatee, formateeName){
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

#' @export
#' @rdname format_key2mtx
format_key2mtx.sndkey_datatype = function(key, formater, formatee, formateeName){
  if(snd::is_snd_item(formater)){
    use_item = snd:::grab_mtxItem(formatee)
    for(i in use_item){
      sel_ColItem = unlist(dplyr::select(.data = formatee, {{i}}))
      sel_Datatype = dplyr::filter(formater, `@item` == i)$`@datatype`
      class(sel_ColItem) = paste0("DT", sel_Datatype)
      sel_ColItem = snd:::format_key2mtx_datatype_use(colItem = sel_ColItem,
                                                      colName = i,
                                                      mtxName = formateeName)
      formatee = dplyr::mutate(.data = formatee, "{i}" := sel_ColItem)
    }
  }
  if(snd::is_snd_factor(formater)){
    use_factor = snd:::grab_mtxFactor(formatee)
    for(i in use_factor){
      Datatype = snd:::grab_keyHead(mtx = dplyr::filter(formater, `@factor` == i),
                                        key = "@datatype")
      colItem = unlist(dplyr::select(.data = formatee, {{i}}))
      class(colItem) = paste0("DT", unique(Datatype))
      colItem = snd:::format_key2mtx_datatype_use(colItem = colItem,
                                                  colName = i,
                                                  mtxName = formateeName)
      formatee = dplyr::mutate(.data = formatee, "{i}" := colItem)
    }
  }
  return(invisible(formatee))
}
