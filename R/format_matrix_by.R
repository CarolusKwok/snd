#' @title
#' System tools: Format data matrix by some other matrix
#'
#' @description
#' A work around based on `snd:::format_matrix` and `snd:::format_key2mtx`
#'
#' @param mtxData The data matrix
#' @param mtxItem The item matrix
#' @param mtxFactor The factor matrix
#' @param mtxName The name of the data matrix
#'
#' @return A formatted Data matrix
#'
#' @keywords internal
#' @rdname formatData_by
formatData_byItem = function(mtxData, mtxItem, mtxName){
  mtxItem = snd:::format_matrix(mtx = mtxItem, mtxName = mtxName)
  mtxData = snd:::format_matrix(mtx = mtxData, mtxName = mtxName)
  use_key = snd:::grab_mtxKey(mtxItem)
  for(i in use_key){
    mtxData = snd:::format_key2mtx(key = i, formater = mtxItem, formatee = mtxData, formateeName = formateeName)
  }
  return(invisible(mtxData))
}

#' @keywords internal
#' @rdname formatData_by
formatData_byFactor = function(mtxData, mtxFactor, mtxName){
  mtxFactor = snd:::format_matrix(mtx = mtxFactor, mtxName = mtxName)
  mtxData = snd:::format_matrix(mtx = mtxData, mtxName = mtxName)
  use_key = snd:::grab_mtxKey(mtxFactor)
  for(i in use_key){
    mtxData = snd:::format_key2mtx(key = i, formater = mtxFactor, formatee = mtxData, formateeName = mtxName)
  }
  return(invisible(mtxData))
}
