#' System tools: Formatting while doing read-in operations
#'
#' @description
#' These functions process the matrix and form them into the final product prior to formating them into a dataset.
#'
#' @param mtx The matrix
#' @param mtxName The name of the matrix
#'
#' @return A dataframe
#' @keywords internal
formatRI_matrix = function(mtx, mtxName){
  snd:::checkRW_matrix(mtx = mtx, mtxName = mtxName)
  UseMethod(generic = "formatRI_matrix", object = mtx)
}

#' @export
formatRI_matrix.default = function(mtx, mtxName){
  snd:::sys_abort(message = c("x" = "Class not supported for {.mtx {mtxName}}",
                              "!" = "Expected class:",
                              "i" = "{.cls snd_data}, {.cls snd_item}, {.cls snd_factor}",
                              "!" = "Current class:",
                              "i" = snd:::sys_message_class(class = class(mtx))),
                  mtxName = mtxName)
}

#' @export
formatRI_matrix.snd_data = function(mtx, mtxName){
  #1. Create @type####
  ava_key = snd:::grab_mtxKey(mtx)
  test_keyType = "@type" %in% ava_key
  if(!test_keyType){
    mtx = dplyr::mutate(.data = mtx, `@type` = "data")
    snd:::sys_warn(message = c("!" = "{.col @type} absent in {.mtx {mtxName}}",
                               "i" = "Created {.col @type} in {.mtx {mtxName}}",
                               "i" = "Assuming all are raw data. {.col @type} is set as {.code data}"),
                   mtxName = mtxName)
  }

  #2. Shuffle####
  mtx = snd:::format_shuffle(mtx = mtx, mtxName = mtxName)

  #3. Format key ####
  for(i in snd:::grab_mtxKey(mtx)){
    mtx = snd:::formatRI_key(key = i, mtx = mtx, mtxName = mtxName)
  }

  #4. Check Factor uniqueness ####
  ava_Factor = snd:::grab_mtxFactor(dataframe = mtx)
  mtx_selected = dplyr::select(.data = dplyr::filter(.data = mtx, `@type` == "data"),
                               dplyr::all_of(ava_Factor))
  if(sum(duplicated(mtx_selected))){
    snd:::sys_warn(message = c("!" = "Non-unique data in {.mtx {mtxName}}",
                               "i" = "Key-Factor combinations are not unique",
                               "i" = "Are you sure that's correct?",
                               "i" = "Factors used:",
                               "i" =  snd:::sys_message_columns(columns = ava_Factor)),
                   x = mtx, mtxName = mtxName)
  }
  #Return ####
  return(invisible(mtx))
}

#' @export
formatRI_matrix.snd_item = function(mtx, mtxName){
  #1. Create @type####
  ava_key = snd:::grab_mtxKey(mtx)
  test_keyType = "@type" %in% ava_key
  if(!test_keyType){
    mtx = dplyr::mutate(.data = mtx, `@type` = "data")
    snd:::sys_warn(message = c("!" = "{.col @type} absent in {.mtx {mtxName}}",
                               "i" = "Created {.col @type} in {.mtx {mtxName}}",
                               "i" = "Assuming all are raw data. {.col @type} is set as {.code data}"),
                   mtxName = mtxName)
  }

  #2. Shuffle ####
  mtx = snd:::format_shuffle(mtx = mtx, mtxName = mtxName)
  #3. Format key ####
  for(i in snd:::grab_mtxKey(mtx)){
    mtx = snd:::formatRI_key(key = i, mtx = mtx, mtxName = mtxName)
  }
  #4. Format factor #NA to na####
  for(i in snd::grab_mtxFactor(mtx)){
    mtx = dplyr::mutate(.data = mtx,
                        "{i}" := ifelse(!!rlang::sym({{i}}) == "#NA",
                                        NA,
                                        !!rlang::sym({{i}})))
  }
  #5. Return ####
  return(invisible(mtx))
}

#' @export
formatRI_matrix.snd_factor = function(mtx, mtxName){
  #1. Shuffle ####
  mtx = snd:::format_shuffle(mtx = mtx, mtxName = mtxName)
  #2. Format key ####
  for(i in snd:::grab_mtxKey(mtx)){
    mtx = snd:::formatRI_key(key = i, mtx = mtx, mtxName = mtxName)
  }
  #3. Format factor #NA to na####
  for(i in snd::grab_mtxFactor(mtx)){
    mtx = dplyr::mutate(.data = mtx,
                        "{i}" := ifelse(!!rlang::sym({{i}}) == "#NA",
                                        NA,
                                        !!rlang::sym({{i}})))
  }
  #Return ####
  return(invisible(mtx))
}
