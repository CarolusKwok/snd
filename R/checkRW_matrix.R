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
  UseMethod(generic = "checkRW_matrix", object = mtx)
}

#' @export
#' @rdname checkRW_matrix
checkRW_matrix.snd_data = function(mtx, mtxName){
  if(!hasArg(mtx))(snd:::sys_abort_NoArg(mtx))
  #Check about the class ####
  if(!is.data.frame(mtx)){snd:::sys_abort_mtxWrongClass(x = mtx, mtxName)}
  #Check if the dataframe columns are unique####
  if(sum(duplicated(colnames(mtx)))){snd:::sys_abort_mtxDuplicatedColumn(x = mtx, name = mtxName)}

  #Check if dataframe consist of >= 1 factor, >= item ####
  ava_key = snd:::sys_grab_dfKey(mtx)
  ava_factor = snd:::sys_grab_dfFactor(mtx)
  ava_item = snd:::sys_grab_dfItem(mtx)

  if(length(ava_factor) <= 0){snd:::sys_abort_mtxMissingFactor(x = mtx, name = mtxName)}
  if(length(ava_item) <= 0){snd:::sys_abort_mtxMissingItem(x = mtx, name = mtxName)}

  #Check if all items column classes make sense ####
  mtx_item = dplyr::select(.data = mtx, {{ava_item}})
  mtx_item_class = unlist(lapply(mtx_item, FUN = class))
  mtx_item_supported = (mtx_item_class %in% snd:::sys_datatype_support())
  if(sum(!(mtx_item_supported))){
    columns = ava_item[!mtx_item_supported]
    snd:::sys_abort_mtxColUnsupport(x = mtx, name = mtxName, columns = columns)
  }
}

#' @export
#' @rdname checkRW_matrix
checkRW_matrix.snd_item = function(mtx, mtxName){
  if(!hasArg(mtx)){snd:::sys_abort_NoArg(mtx)}
  #Check about the class ####
  if(!is.data.frame(mtx)){snd:::sys_abort_mtxWrongClass(x = mtx, name = mtxName)}

  #Check if dataframe columns are unique ####
  if(sum(duplicated(colnames(mtx)))){snd:::sys_abort_mtxDuplicatedColumn(x = mtx, name = mtxName)}

  #Check if the dataframe contains @item	@datatype ####
  ava_key = snd:::sys_grab_dfKey(mtx)
  keyRequired = c("@item", "@datatype")
  keyTested = keyRequired %in% ava_key
  keyMissing = keyRequired[!keyTested]
  if(sum(!keyTested)){
    snd:::sys_abort_mtxMissingKey(x = mtx,
                                      keys_missing = keyMissing,
                                      name = mtxName)
  }

  #Check if @datatype are supported ####
  sel_mtx = dplyr::mutate(.data = mtx,
                  dash_position = stringr::str_locate(`@datatype`, pattern = "_")[, "start"],
                  dash_position = ifelse(is.na(dash_position), -1L, dash_position),
                  real_datatype = stringr::str_sub(string = `@datatype`, start = 1L, end = dash_position))
  datatype_elements = sel_mtx$real_datatype
  datatype_full = snd:::sys_datatype_support(with_abbr = T)$full
  datatype_alias = snd:::sys_datatype_support(with_abbr = T)$alias
  datatype_abbr = snd:::sys_datatype_support(with_abbr = T)$abbr

  test_datatype = (datatype_elements %in% c(datatype_full, datatype_alias, datatype_abbr))
  if(sum(!test_datatype)){
    unsupport_datatype = unique(datatype_elements[!test_datatype])
    snd:::sys_abort_mtxKeyDatatypeUnsupported(x = mtx,
                                              name = mtxName,
                                              unsupport_datatype = unsupport_datatype)
  }
}

#' @export
#' @rdname checkRW_matrix
checkRW_matrix.snd_factor = function(mtx, mtxName){
  if(!hasArg(mtx)){snd:::sys_abort_NoArg(mtx)}
  #Check about the class ####
  if(!is.data.frame(mtx)){snd:::sys_abort_mtxWrongClass(x = mtx, name = mtxName)}

  #Check if dataframe columns are unique ####
  if(sum(duplicated(colnames(mtx)))){snd:::sys_abort_mtxDuplicatedColumn(x = mtx, name = mtxName)}

  #Check if the dataframe contains @factor @datatype @label ####
  ava_key = snd:::sys_grab_dfKey(mtx)
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

  #Check if everything in @factor has a `#` prefix ####
  key_Factor = mtx$`@factor`
  key_Label = mtx$`@label`
  key_Abbr = mtx$`@abbr`

  key_FactorTest = stringr::str_detect(string = stringr::str_sub(string = key_Factor, start = 1L, end = 1L),
                                       pattern = "#", negate = TRUE)
  key_FactorFailed = key_Factor[key_FactorTest]
  if(sum(key_FactorTest)){
    snd:::sys_abort_mtxColContainNonFactor(x = mtx, name = mtxName, columns = key_FactorFailed)
  }
  #Check if all @factor and @label are filled ####
  key_FactorTestNA = is.na(key_Factor)
  key_LabelTestNA =  is.na(key_Label)

  if(sum(key_FactorTestNA) | sum(key_LabelTestNA)){
    keys_Failed = c()
    if(sum(key_FactorTestNA)){keys_Failed = c(keys_Failed, "@factor")}
    if(sum(key_LabelTestNA)){keys_Failed = c(keys_Failed, "@label")}
    snd:::sys_abort_mtxColNotAllFilled(x = mtx, name = mtxName, columns = keys_Failed)
  }

  #Separate mtx by @factor for further process ####
  seperate_byFactor = function(x, df){
    return(dplyr::filter(.data = df, `@factor` == x))
  }
  keyFactor_unique = unique(key_Factor)
  mtx_seperated = lapply(X = keyFactor_unique, FUN = seperate_byFactor, df = mtx)

  #Check if all @label under the same @factor are unique####
  Labels_test = function(df){
    return(sum(duplicated(df$`@label`)))
  }
  keyLabel_test = unlist(lapply(X = mtx_seperated, FUN = Labels_test))

  if(sum(keyLabel_test)){
    failed_groups = keyFactor_unique[keyLabel_test]
    snd:::sys_abort_mtxColGrpItemsNotUnique(x = mtx,
                                            name = mtxName,
                                            columns = "@label",
                                            group_by = "@factor",
                                            failed_groups = failed_groups)
  }

  #Check if head of @datatype under the same @factor are the same ####
  Factors_test = function(df){
    df = dplyr::mutate(.data = df,
                       head = snd:::sys_grab_keyHead(mtx = df, key = "@datatype"))
    unique = unique(df$head)
    return((length(unique) != 1))
  }
  keyFactor_test = unlist(lapply(X = mtx_seperated, FUN = Factors_test))
  if(sum(keyFactor_test)){
    failed_groups = keyFactor_unique[keyFactor_test]
    snd:::sys_abort_mtxColGrpItemsNotSame(x = mtx,
                                          name = mtxName,
                                          columns = "@datatype",
                                          group_by = "@factor",
                                          failed_groups = failed_groups)
  }
}
