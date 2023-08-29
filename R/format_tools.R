#' @keywords internal
#' @rdname format_matrix
format_MakeKeyType = function(mtx, mtxName){
  ava_key = snd:::grab_mtxKey(mtx)
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
  ava_Key = snd:::grab_mtxKey(dataframe = mtx)
  ava_Factor = snd:::grab_mtxFactor(dataframe = mtx)
  mtx = dplyr::relocate(.data = mtx, dplyr::all_of(ava_Key), dplyr::all_of(ava_Factor))
}
