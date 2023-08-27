#' Title
#'
#' @param mtxData
#' @param mtxItem
#' @param mtxFactor
#' @param mtxName
#'
#' @return A formatted Data matrix
#'
#' @keywords internal
#' @rdname formatData_by
formatData_byItem = function(mtxData, mtxItem, mtxName){
  mtxItem = snd:::format_matrix(mtx = mtxItem, mtxName = mtxName)
  mtxData = snd:::format_matrix(mtx = mtxData, mtxName = mtxName)
  use_key = snd:::sys_grab_dfKey(mtxItem)
  for(i in use_key){
    mtxData = snd:::format_withkey(key = i, formater = mtxItem, formatee = mtxData, formateeName = formateeName)
  }
  return(invisible(mtxData))
}

#' @keywords internal
#' @rdname formatData_by
formatData_byFactor = function(mtxData, mtxFactor, mtxName){
  mtxFactor = snd:::format_matrix(mtx = mtxFactor, mtxName = mtxName)
  mtxData = snd:::format_matrix(mtx = mtxData, mtxName = mtxName)
  use_key = snd:::sys_grab_dfKey(mtxFactor)
  for(i in use_key){
    mtxData = snd:::format_withkey(key = i, formater = mtxFactor, formatee = mtxData, formateeName = mtxName)
  }
  return(invisible(mtxData))
}
