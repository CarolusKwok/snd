#' @title
#' System tools: Precheck for r/w data matrix
#'
#' @description
#' Prior to read-in a matrix, the matrix must loosely resemble the final snd product. These functions check if the matrix fits those criteria to be transformed into a snd matrix.
#'
#' @param mtx The matrix
#' @param mtxName The name of the matrix
#'
#' @return A standard abort message, if applicable
#' @keywords Internal
#' @rdname checkRW_matrix
checkRW_matrix = function(mtx, mtxName){
  #Checks ####
  if(rlang::is_missing(mtx))(snd:::sys_abort_NoArg(mtx))
  if(rlang::is_missing(mtxName)){snd:::sys_abort_NoArg(mtxName)}
  if(!is.data.frame(mtx)){
    class = class(mtx)
    snd:::sys_abort(message = c("x" = "Wrong class for {.mtx {mtxName}}",
                                "!" = "You've supplied {.cls {class}}",
                                "i" = "Please use {.cls data.frame}/ {.cls tibble} for {.mtx {mtxName}}"),
                    arg = rlang::caller_arg(arg = mtx), mtxName = mtxName, class = class)
  }
  if(sum(duplicated(colnames(mtx)))){
    colnames = colnames(mtx)
    dup_columns = stringr::str_flatten(string = paste0('{.col ', unique(colnames[duplicated(colnames)]) , '}'),
                                       collapse = ", ")
    snd:::sys_abort(message = c("x" = "Duplicated columns in {.mtx {mtxName}}",
                                "i" = "Please check columns in {.mtx {mtxName}}",
                                "i" = "Duplicated Columns:",
                                "i" = dup_columns),
                    mtxName = mtxName)
    }

  #Start ####
  UseMethod(generic = "checkRW_matrix", object = mtx)
}

#' @export
checkRW_matrix.snd_data = function(mtx, mtxName){
  #Check if dataframe consist of >= 1 factor, >= item ####
  ava_key = snd:::grab_mtxKey(mtx)
  ava_factor = snd:::grab_mtxFactor(mtx)
  ava_item = snd:::grab_mtxItem(mtx)
  if(length(ava_factor) <= 0){
    snd:::sys_abort(message = c("x" = "Missing factors in {.mtx {mtxName}}",
                                "i" = "Please include factors in {.mtx {mtxName}}"),
                    mtxName = mtxName)
  }

  if(length(ava_item) <= 0){
    snd:::sys_abort(message = c("x" = "Missing items in {.mtx {mtxName}}",
                                "i" = "Please include items in {.mtx {mtxName}}"),
                    mtxName = mtxName)}
}

#' @export
checkRW_matrix.snd_item = function(mtx, mtxName){
  #Check if the dataframe contains @item	@format ####
  ava_key = snd:::grab_mtxKey(mtx)
  keyRequired = c("@item", "@format")
  keyTested = keyRequired %in% ava_key
  keyMissing = keyRequired[!keyTested]
  if(sum(!keyTested)){
    snd:::sys_abort_mtxMissingSelectedKey(x = mtx,
                                      keys_missing = keyMissing,
                                      name = mtxName)
  }
}

#' @export
checkRW_matrix.snd_factor = function(mtx, mtxName){
  #Check if the dataframe contains @factor @format @label ####
  ava_key = snd:::grab_mtxKey(mtx)
  keyRequired = c("@factor", "@format", "@label")
  keyTested = keyRequired %in% ava_key
  keyMissing = keyRequired[!keyTested]
  if(sum(!keyTested)){
    snd:::sys_abort_mtxMissingSelectedKey(x = mtx,
                                  keys_missing = keyMissing,
                                  name = mtxName)
  }

  #Check if all columns are characters ####
  colClass = sapply(mtx, FUN = class, simplify = TRUE, USE.NAMES = FALSE)
  colClassTest = (colClass != "character")
  if(sum(colClassTest)){
    columns = stringr::str_flatten(paste0("{.col ", colnames(x = mtx)[colClassTest], "}"),
                                   collapse = ", ")
    snd:::sys_abort(message = c("x" = "Wrong class for columns in {.mtx {mtxName}}",
                                "!" = "Columns with wrong class include:",
                                "!" = columns,
                                "i" = "Please use the following classes for those columns in {.mtx {mtxName}}:",
                                "i" = "{.cls character}"),
                    mtxName = mtxName)
  }
}
