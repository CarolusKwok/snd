#' @export
#' @rdname formatRI_key2mtx
formatRI_key2mtx.sndkey_label = function(key, formater, formatee, formateeName){
  #UseMethod(generic = "formatRI_key2mtx.sndkey_label", object = formater)
  if("snd_factor" %in% class(formater)){
    #Check ####
    if(!("@factor" %in% colnames(formater))){snd:::sys_abort_mtxMissingSelectedKey(x = formater, keys_missing = "@factor", name = formateeName)}
    if(!("@label" %in% colnames(formater))){snd:::sys_abort_mtxMissingSelectedKey(x = formater, keys_missing = "@label", name = formateeName)}
    #Start using label as key
    unique_elements = unique(formater$`@factor`)
    used_elements = unique_elements[unique_elements %in% colnames(formatee)]
    seperated_formater = lapply(X = used_elements,
                                FUN = function(X, df){return(dplyr::filter(.data = df, `@factor` == X)$`@label`)}, df = formater)
    test = vector(length = length(used_elements)) #TRUE = pass; FALSE = fail
    for(i in 1:length(used_elements)){
      expected_input = unlist(seperated_formater[i])
      selected_formatee = dplyr::select(.data = formatee, !!rlang::sym({{used_elements[[i]]}}))
      colnames(selected_formatee) = "input"
      selected_formatee = dplyr::mutate(.data = selected_formatee,
                                        in_expected = ifelse("###" %in% expected_input, TRUE, (input %in% expected_input)))
      test[i] = (sum(selected_formatee$in_expected) == nrow(selected_formatee))
    }
    if(sum(!test)){
      failed_columns = used_elements[!test]
      snd:::sys_abort_mtxKeyLabelIncorrectlyDescribeData(x = formatee,
                                                         name = formateeName,
                                                         failed_columns = failed_columns)
    }
    #Return if OK
    return(invisible(formatee))
  }
  if("snd_item" %in% class(formater)){
    if(!("@item" %in% colnames(formater))){snd:::sys_abort_mtxMissingSelectedKey(x = formater, keys_missing = "@item", name = formateeName)}
    if(!("@label" %in% colnames(formater))){snd:::sys_abort_mtxMissingSelectedKey(x = formater, keys_missing = "@label", name = formateeName)}

    #Start using label as key
    unique_elements = unique(formater$`@item`)
    used_elements = unique_elements[unique_elements %in% colnames(formatee)]
    seperated_formater = lapply(X = used_elements,
                                FUN = function(X, df){return(dplyr::filter(.data = df, `@item` == X)$`@label`)}, df = formater)
    test = vector(length = length(used_elements)) #TRUE = pass; FALSE = fail
    for(i in 1:length(used_elements)){
      expected_input = unlist(seperated_formater[i])
      selected_formatee = dplyr::select(.data = formatee, input = !!rlang::sym({{used_elements[i]}})) %>%
        dplyr::mutate(in_expected = ifelse("###" %in% expected_input, TRUE, input %in% expected_input))
      test[i] = (sum(selected_formatee$in_expected) == nrow(selected_formatee))
    }
    if(sum(!test)){
      failed_columns = used_elements[!test]
      snd:::sys_abort_mtxKeyLabelIncorrectlyDescribeData(x = formatee,
                                                         name = formateeName,
                                                         failed_columns = failed_columns)
    }
    #Return if OK
    return(invisible(formatee))
  }
}
