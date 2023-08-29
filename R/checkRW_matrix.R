#' @title
#' System tools: Precheck for r/w data matrix
#'
#' @description
#' Prior to read-in a matrix, the matrix must loosely resemble the final snd product. These functions check if the matrix fits those criteria to be transformed into a snd matrix.
#'
#' @param mtx The matrix
#' @param mtxName The name of the matrix
#'
#' @return A message
#' @keywords Internal
#'
#' @rdname checkRW_matrix
checkRW_matrix = function(mtx, mtxName){
  #Checks ####
  if(!hasArg(mtx))(snd:::sys_abort_NoArg(mtx))
  if(!is.data.frame(mtx)){snd:::sys_abort_mtxWrongClass(x = mtx, mtxName)}
  if(sum(duplicated(colnames(mtx)))){snd:::sys_abort_mtxDuplicatedColumn(x = mtx, name = mtxName)}

  #Start ####
  UseMethod(generic = "checkRW_matrix", object = mtx)
}

#' @export
#' @rdname checkRW_matrix
checkRW_matrix.snd_data = function(mtx, mtxName){
  #Check if dataframe consist of >= 1 factor, >= item ####
  ava_key = snd:::grab_mtxKey(mtx)
  ava_factor = snd:::grab_mtxFactor(mtx)
  ava_item = snd:::grab_mtxItem(mtx)
  if(length(ava_factor) <= 0){snd:::sys_abort_mtxMissingFactor(x = mtx, name = mtxName)}
  if(length(ava_item) <= 0){snd:::sys_abort_mtxMissingItem(x = mtx, name = mtxName)}
}

#' @export
#' @rdname checkRW_matrix
checkRW_matrix.snd_item = function(mtx, mtxName){
  #Check if the dataframe contains @item	@datatype ####
  ava_key = snd:::grab_mtxKey(mtx)
  keyRequired = c("@item", "@datatype")
  keyTested = keyRequired %in% ava_key
  keyMissing = keyRequired[!keyTested]
  if(sum(!keyTested)){
    snd:::sys_abort_mtxMissingKey(x = mtx,
                                      keys_missing = keyMissing,
                                      name = mtxName)
  }
}

#' @export
#' @rdname checkRW_matrix
checkRW_matrix.snd_factor = function(mtx, mtxName){
  #Check if the dataframe contains @factor @datatype @label ####
  ava_key = snd:::grab_mtxKey(mtx)
  keyRequired = c("@factor", "@datatype", "@label")
  keyTested = keyRequired %in% ava_key
  keyMissing = keyRequired[!keyTested]
  if(sum(!keyTested)){
    snd:::sys_abort_mtxMissingKey(x = mtx,
                                  keys_missing = keyMissing,
                                  name = mtxName)
  }

  #Check if all columns are characters ####
  colClass = unlist(lapply(mtx, FUN = class))
  colClassTest = (colClass != "character")
  colFailed = colnames(x = mtx)[colClassTest]
  if(sum(colClassTest)){
    snd:::sys_abort_mtxColWrongClass(x = mtx, name = mtxName, columns = colFailed, expected = "character")
  }
}
