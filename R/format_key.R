#' Trial
#'
#' @param key
#' @param mtx
#' @param mtxName
#'
#' @return
#' @keywords internal
format_key = function(key, mtx, mtxName){
  key = stringr::str_sub(string = key, start = 2L, end = -1L)
  fun_text = paste0("snd:::format_key_", key, "(mtx = mtx, mtxName = mtxName)")
  return(eval(parse(text = fun_text)))
}

#' @keywords internal
#' @rdname format_key
format_key_type = function(mtx, mtxName){
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

#' @keywords internal
#' @rdname format_key
format_key_item = function(mtx, mtxName){
  return(mtx)
}

#' @keywords internal
#' @rdname format_key
format_key_datatype = function(mtx, mtxName){
  #Format ####
  head = snd:::sys_grab_keyHead(mtx = mtx, key = "@datatype")
  tail = snd:::sys_grab_keyTail(mtx = mtx, key = "@datatype")
  sys_datatype = snd:::sys_datatype_support(with_abbr = TRUE)
  if(snd:::is_snd_factor(mtx)){formated_dt = tibble::tibble(elements = mtx$`@factor`)}
  if(snd:::is_snd_item(mtx)){formated_dt = tibble::tibble(elements = mtx$`@item`)}
  formated_dt = dplyr::mutate(.data = formated_dt,
                              DT = mtx$`@datatype`,
                              head = head,
                              tail = tail,
                              head = ifelse(is.na(head), "factor", head),
                              match_abbr = match(x = head, table = sys_datatype$abbr),
                              match_alias = match(x = head, table = sys_datatype$alias),
                              match_full = match(x = head, table = sys_datatype$full),
                              match_abbr = ifelse(is.na(match_abbr), 0, match_abbr),
                              match_alias = ifelse(is.na(match_alias), 0, match_alias),
                              match_full = ifelse(is.na(match_full), 0, match_full),
                              match = match_abbr + match_alias + match_full)
  #Check point 1, return error if there is no match ####
  if(sum(formated_dt$match == 0)){
    unsupported = dplyr::filter(.data = formated_dt, match == 0)$DT
    snd:::sys_abort_mtxKeyDatatypeUnsupported(x = mtx,
                                              name = mtxName,
                                              unsupport_datatype = unsupported)
  }

  #Continue formatting ####
  formated_dt = formated_dt %>%
    dplyr::mutate(fullname = sys_datatype$full[match],
                  with_tail = ifelse(is.na(tail),
                                     fullname,
                                     paste0(fullname, "_", tail)))

  #Check point 2, return error if fullname is inconsistant with @factor ####
  elements = unique(formated_dt$elements)
  mtx_seperated = lapply(X = elements,
                         FUN = function(X, df){return(dplyr::filter(.data = df, elements == X))},
                         df = formated_dt)
  test = lapply(X = mtx_seperated,
                FUN = function(X){return(length(unique(X$fullname)) == 1)}) %>%
    unlist()
  if(sum(!test)){
    failed_groups = elements[!test]

    snd:::sys_abort_mtxColGrpItemsNotSame(x = mtx,
                                          name = mtxName,
                                          columns = "@datatype",
                                          group_by = "@factor",
                                          failed_groups = failed_groups)
  }
  #Return everything with the formatted datatype ####
  mtx = dplyr::mutate(.data = mtx,
                      `@datatype` := formated_dt$with_tail)
  return(mtx)
}


#' @keywords internal
#' @rdname format_key
format_key_factor = function(mtx, mtxName){
  return(mtx)
}

#' @keywords internal
#' @rdname format_key
format_key_label = function(mtx, mtxName){
  return(mtx)
}
