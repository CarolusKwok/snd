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
#'
#' @rdname format_matrix
format_matrix = function(mtx, mtxName){
  UseMethod(generic = "format_matrix", object = mtx)
}

#' @export
#' @rdname format_matrix
format_matrix.snd_data = function(mtx, mtxName){
  if(!hasArg(mtx)){snd:::sys_abort_NoArg(mtx)}
  snd:::checkRW_matrix(mtx = mtx, mtxName = mtxName)

  #Start!!! ####
  ##Create @type if absent ####
  mtx = snd:::format_MakeKeyType(mtx = mtx, mtxName = mtxName)

  #Shuffle all Keys and Factors to be in front of every item ####
  mtx = snd:::format_shuffle(mtx = mtx, mtxName = mtxName)

  #Format every key within the matrix ####
  for(i in snd:::sys_grab_dfKey(mtx)){
    mtx = snd:::format_key(key = i, mtx = mtx, mtxName = mtxName)
  }

  ###Check uniqueness of Key and Factor, warn only ####
  ava_Key = snd:::sys_grab_dfKey(dataframe = mtx)
  ava_Factor = snd:::sys_grab_dfFactor(dataframe = mtx)
  mtx_selected = dplyr::select(.data = mtx, dplyr::all_of(ava_Key), dplyr::all_of(ava_Factor))
  mtx_selected_distinct = dplyr::distinct(.data = mtx_selected)
  if(nrow(mtx_selected) != nrow(mtx_selected_distinct)){
    print_ava_Factor = stringr::str_flatten(string = paste0("{.col ", ava_Factor, "}"), collapse = ", ")
    if(!hasArg(mtxName)){
      snd:::sys_warn(message = c("!" = "Non-unique data in {.arg {arg}}",
                                 "i" = "Key-Factor combinations are not unique",
                                 "i" = "Are you sure that's correct?",
                                 "i" = "Factors used:",
                                 "i" = print_ava_Factor),
                     x = mtx)
    } else {
      snd:::sys_warn(message = c("!" = "Non-unique data in {.mtx {mtxName}}",
                                 "i" = "Key-Factor combinations are not unique",
                                 "i" = "Are you sure that's correct?",
                                 "i" = "Factors used:",
                                 "i" = print_ava_Factor),
                     x = mtx, mtxName = mtxName)
    }
  }
  #Return ####
  return(invisible(mtx))
}

#' @export
#' @rdname format_matrix
format_matrix.snd_item = function(mtx, mtxName){
  if(!hasArg(mtx)){snd:::sys_abort_NoArg(mtx)}
  snd:::checkRW_matrix(mtx = mtx, mtxName = mtxName)
  #Create @type if absent####
  mtx = snd:::format_MakeKeyType(mtx = mtx, mtxName = mtxName)
  #Shuffle all Keys and Factors to be in front of every item ####
  mtx = snd:::format_shuffle(mtx = mtx, mtxName = mtxName)
  #Format every key within the matrix ####
  for(i in snd:::sys_grab_dfKey(mtx)){
    mtx = snd:::format_key(key = i, mtx = mtx, mtxName = mtxName)
  }
  #Return ####
  return(invisible(mtx))
}

#' @export
#' @rdname format_matrix
format_matrix.snd_factor = function(mtx, mtxName){
  if(!hasArg(mtx)){snd:::sys_abort_NoArg(mtx)}
  snd:::checkRW_matrix(mtx = mtx, mtxName = mtxName)

  #Shuffle all Keys and Factors to be in front of every item ####
  mtx = snd:::format_shuffle(mtx = mtx, mtxName = mtxName)
  ##Convert @datatype back into full name ####
  for(i in snd:::sys_grab_dfKey(mtx)){
    mtx = snd:::format_key(key = i, mtx = mtx, mtxName = mtxName)
  }
  #Return ####
  return(invisible(mtx))
}
