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
#' @examples
checkRW_matrix = function(mtx, mtxName){
  #0. Check input####
  if(rlang::is_missing(mtx))(snd:::sys_abort_NoArg(mtx))
  if(rlang::is_missing(mtxName)){snd:::sys_abort_NoArg(mtxName)}

  #1. Check data.frame #####
  if(!is.data.frame(mtx)){
    snd:::sys_abort(message = c("x" = "Wrong class for {.mtx {mtxName}}",
                                "!" = "Expected class:",
                                "i" = "{.cls data.frame}, {.cls tibble}",
                                "!" = "Current class:",
                                "i" = "{.cls {class}}"),
                    mtxName = mtxName, class = class(mtx))
  }

  #2. Check duplicated column names ####
  colnames = colnames(mtx)
  test = duplicated(colnames)
  if(sum(test)){
    snd:::sys_abort(message = c("x" = "Duplicated column names in {.mtx {mtxName}}",
                                "!" = "Duplicated Column names:",
                                "i" = snd:::sys_message_columns(columns = unique(colnames[test]))),
                    mtxName = mtxName)
    }

  #Start ####
  UseMethod(generic = "checkRW_matrix", object = mtx)
}

#' @export
checkRW_matrix.default = function(mtx, mtxName){
  snd:::sys_abort(message = c("x" = "Class not supported for {.mtx {mtxName}}",
                              "!" = "Expected class:",
                              "i" = "{.cls snd_data}, {.cls snd_item}, {.cls snd_factor}",
                              "!" = "Current class:",
                              "i" = snd:::sys_message_class(class = class(mtx))),
                  mtxName = mtxName)
}

#' @export
checkRW_matrix.snd_data = function(mtx, mtxName){
  ava_key = snd:::grab_mtxKey(mtx)
  ava_factor = snd:::grab_mtxFactor(mtx)
  ava_item = snd:::grab_mtxItem(mtx)

  #1. Check if dataframe consist of >= 1 factor, >= item ####
  test_factor = (length(ava_factor) == 0)
  test_item = (length(ava_item) == 0)
  if(test_factor & test_item){
    snd:::sys_abort(message = c("x" = "Missing factors & item in {.mtx {mtxName}}",
                                "i" = "{.mtx {mtxName}} is a {.cls snd_data}",
                                "i" = "{.cls snd_data} should include both columns of {.col factors} & {.col items}",
                                "i" = "Include {.col factors} by adding columns with {.code #} prefix",
                                "i" = "Include {.col items} by adding columns without {.code #} prefix"),
                    mtxName = mtxName)
    } else if (test_factor) {
      snd:::sys_abort(message = c("x" = "Missing factors in {.mtx {mtxName}}",
                                  "i" = "{.mtx {mtxName}} is a {.cls snd_data}",
                                  "i" = "{.cls snd_data} should include columns of {.col factors}",
                                  "i" = "Include {.col factors} by adding columns with {.code #} prefix"),
                      mtxName = mtxName)
    } else if (test_item) {
      snd:::sys_abort(message = c("x" = "Missing items in {.mtx {mtxName}}",
                                  "i" = "{.mtx {mtxName}} is a {.cls snd_data}",
                                  "i" = "{.cls snd_data} should include columns of {.col items}",
                                  "i" = "Include {.col items} by adding columns without {.code #} prefix"),
                      mtxName = mtxName)
    }
}

#' @export
checkRW_matrix.snd_item = function(mtx, mtxName){
  #1. Check if the dataframe contains @item @format ####
  ava_key = snd:::grab_mtxKey(mtx)
  keyRequired = c("@item", "@format")
  keyTested = keyRequired %in% ava_key
  if(sum(!keyTested)){
    keyMissing = keyRequired[!keyTested]
    snd:::sys_abort_mtxMissingSelectedKey(x = mtx,
                                          keys_missing = keyMissing,
                                          name = mtxName)
  }

  #2. Check if the dataframe contain items ####
  ava_item = snd:::grab_mtxItem(mtx)
  if(length(ava_item) > 0){
    snd:::sys_abort(message = c("x" = "Containing items in {.mtx {mtxName}}",
                                "i" = "{.mtx {mtxName}} is a {.cls snd_item}",
                                "!" = "{.cls snd_item} should not contains {.col items}",
                                "i" = "Item columns:",
                                "i" = snd:::sys_message_columns(columns = ava_item),
                                "i" = "Turn it into a factor with the {.code #} prefix"),
                    mtxName = mtxName)
  }
}

#' @export
checkRW_matrix.snd_factor = function(mtx, mtxName){
  #1. Check if the dataframe contains @factor @format @label ####
  ava_key = snd:::grab_mtxKey(mtx)
  keyRequired = c("@factor", "@format", "@label")
  keyTested = keyRequired %in% ava_key
  if(sum(!keyTested)){
    keyMissing = keyRequired[!keyTested]
    snd:::sys_abort_mtxMissingSelectedKey(x = mtx,
                                          keys_missing = keyMissing,
                                          name = mtxName)
  }

  #2. Check if all columns are characters ####
  colClass = sapply(mtx, FUN = class, simplify = TRUE, USE.NAMES = FALSE)
  colClassTest = (colClass != "character")
  if(sum(colClassTest)){
    snd:::sys_abort(message = c("x" = "Wrong class for columns in {.mtx {mtxName}}",
                                "!" = "Columns with wrong class:",
                                "!" = snd:::sys_message_columns(columns = colnames(x = mtx)[colClassTest]),
                                "i" = "Expected class:",
                                "i" = "{.cls character}"),
                    mtxName = mtxName)
  }

  #3. Check if dataframe contain items ####
  ava_item = snd:::grab_mtxItem(mtx)
  if(length(ava_item) > 0){
    snd:::sys_abort(message = c("x" = "Containing items in {.mtx {mtxName}}",
                                "i" = "{.mtx {mtxName}} is a {.cls snd_item}",
                                "!" = "{.cls snd_item} should not contains {.col items}",
                                "!" = "Item columns:",
                                "!" = snd:::sys_message_columns(columns = ava_item),
                                "i" = "Turn it into a factor with the {.code #} prefix"),
                    mtxName = mtxName)
  }
}
