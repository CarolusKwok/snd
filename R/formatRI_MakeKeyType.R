#' @keywords internal
#' @rdname formatRI_matrix
formatRI_MakeKeyType = function(mtx, mtxName){
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
