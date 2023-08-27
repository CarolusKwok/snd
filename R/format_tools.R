#' @keywords internal
#' @rdname format_matrix
format_MakeKeyType = function(mtx, mtxName){
  ava_key = snd:::sys_grab_dfKey(mtx)
  key_typeTest = "@type" %in% ava_key
  if(!key_typeTest){
    mtx = dplyr::mutate(.data = mtx,
                        `@type` = "data")
    if(rlang::is_missing(mtxName)){
      snd:::sys_warn(message = c("!" = "{.col @type} absent in {.arg {arg}}",
                                 "i" = "Created {.col @type} in {.arg {arg}}",
                                 "i" = "Assuming all are raw data. {.col @type} is set as {.code data}"),
                     x = mtx)
    } else {
      snd:::sys_warn(message = c("!" = "{.col @type} absent in {.mtx {mtxName}}",
                                 "i" = "Created {.col @type} in {.mtx {mtxName}}",
                                 "i" = "Assuming all are raw data. {.col @type} is set as {.code data}"),
                     x = mtx, mtxName = mtxName)
    }
  }
  return(mtx)
}

#' @keywords internal
#' @rdname format_matrix
format_shuffle = function(mtx, mtxName){
  ava_Key = snd:::sys_grab_dfKey(dataframe = mtx)
  ava_Factor = snd:::sys_grab_dfFactor(dataframe = mtx)
  mtx = dplyr::relocate(.data = mtx, dplyr::all_of(ava_Key), dplyr::all_of(ava_Factor))
}


#'
#' #' @keywords internal
#' #' @rdname format_matrix
#' format_keyDatatype = function(mtx){
#'   head = snd:::sys_grab_keyHead(mtx = mtx, key = "@datatype")
#'   tail = snd:::sys_grab_keyTail(mtx = mtx, key = "@datatype")
#'
#'   ava_datatype = snd:::sys_datatype_support(with_abbr = TRUE)
#'   DT_full = ava_datatype$full
#'   DT_alias = ava_datatype$alias
#'   DT_abbr = ava_datatype$abbr
#'
#'   sel_mtx = dplyr::select(.data = mtx,
#'                           key = `@datatype`) %>%
#'     dplyr::mutate(head = head,
#'                   tail = tail,
#'                   match_full = match(x = head, table = DT_full),
#'                   match_full = ifelse(is.na(match_full), 0, match_full),
#'                   match_alias = match(x = head, table = DT_alias),
#'                   match_alias = ifelse(is.na(match_alias), 0, match_alias),
#'                   match_abbr = match(x = head, table = DT_abbr),
#'                   match_abbr = ifelse(is.na(match_abbr), 0, match_abbr),
#'                   match = match_full + match_alias + match_abbr,
#'                   fullname = ifelse(is.na(tail),
#'                                     DT_full[match],
#'                                     paste0(DT_full[match], "_", tail)))
#'
#'   print(sel_mtx)
#'
#'   mtx = dplyr::mutate(.data = mtx, `@datatype` = sel_mtx$fullname)
#'   return(mtx)
#' }
