#' @keywords internal
#' @rdname formatRI_matrix
format_shuffle = function(mtx, mtxName){
  #Part 1: Grab the keys and sort it according to priority ####
  keys = snd:::grab_mtxKey(dataframe = mtx)
  key_priority = sapply(X = lapply(keys, FUN = snd:::classify_key), FUN = snd:::key_priority, simplify = TRUE) %>%
    setNames(nm = keys) %>%
    sort(decreasing = TRUE)
  keys = names(key_priority)

  #Part 2: Grab the factors ####
  factors = snd:::grab_mtxFactor(dataframe = mtx)

  #Part 3: Sort it according to relocate ####
  return(invisible(dplyr::relocate(.data = mtx, dplyr::all_of(keys), dplyr::all_of(factors))))
}



