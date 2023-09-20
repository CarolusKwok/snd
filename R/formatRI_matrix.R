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
  if(!hasArg(mtx)){snd:::sys_abort_NoArg(mtx)}
  snd:::checkRW_matrix(mtx = mtx, mtxName = mtxName)
  UseMethod(generic = "formatRI_matrix", object = mtx)
}

#' @export
formatRI_matrix.snd_data = function(mtx, mtxName){
  ##Create @type if absent ####
  mtx = snd:::formatRI_MakeKeyType(mtx = mtx, mtxName = mtxName)

  #Shuffle all Keys and Factors to be in front of every item ####
  mtx = snd:::format_shuffle(mtx = mtx, mtxName = mtxName)

  #Format every key within the matrix ####
  for(i in snd:::grab_mtxKey(mtx)){
    mtx = snd:::formatRI_key(key = i, mtx = mtx, mtxName = mtxName)
  }

  ###Check uniqueness of Key and Factor, warn only ####
  ava_Key = snd:::grab_mtxKey(dataframe = mtx)
  ava_Factor = snd:::grab_mtxFactor(dataframe = mtx)
  mtx_selected = dplyr::select(.data = mtx, dplyr::all_of(ava_Key), dplyr::all_of(ava_Factor))
  mtx_selected_distinct = dplyr::distinct(.data = mtx_selected)
  if(nrow(mtx_selected) != nrow(mtx_selected_distinct)){
    print_ava_Factor = stringr::str_flatten(string = paste0("{.col ", ava_Factor, "}"), collapse = ", ")
    if(rlang::is_missing(mtxName)){
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
formatRI_matrix.snd_item = function(mtx, mtxName){
  #Create @type if absent####
  mtx = snd:::formatRI_MakeKeyType(mtx = mtx, mtxName = mtxName)
  #Shuffle all Keys and Factors to be in front of every item ####
  mtx = snd:::format_shuffle(mtx = mtx, mtxName = mtxName)
  #Format every key within the matrix ####
  for(i in snd:::grab_mtxKey(mtx)){
    mtx = snd:::formatRI_key(key = i, mtx = mtx, mtxName = mtxName)
  }
  #Format every factor so that #NA is na ####
  for(i in snd::grab_mtxFactor(mtx)){
    mtx = dplyr::mutate(.data = mtx,
                        "{i}" := ifelse(!!rlang::sym({{i}}) == "#NA",
                                        NA,
                                        !!rlang::sym({{i}})))
  }
  #Return ####
  return(invisible(mtx))
}

#' @export
formatRI_matrix.snd_factor = function(mtx, mtxName){
  #Shuffle all Keys and Factors to be in front of every item ####
  mtx = snd:::format_shuffle(mtx = mtx, mtxName = mtxName)
  #Format every key within the matrix ####
  for(i in snd:::grab_mtxKey(mtx)){
    mtx = snd:::formatRI_key(key = i, mtx = mtx, mtxName = mtxName)
  }
  #Return ####
  return(invisible(mtx))
}
