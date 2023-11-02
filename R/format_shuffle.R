#' System tools: Shuffle matrix columns.
#'
#' @description
#' In `snd`, how the matrixes are read are importants as it dictates which keys work first. This program first shuffles all the matrix and determines what is read first by using the simple to use `snd:::key_priority` function. A higher priority, the faster it will be read.
#'
#' @param mtx A matrix of any kind
#' @param mtxName Name of the matrix
#'
#' @keywords internal
#' @example
format_shuffle = function(mtx, mtxName){
  #0. Internal checks ####
  if(rlang::is_missing(mtx)){snd:::sys_abort_NoArg(mtx)}
  if(rlang::is_missing(mtxName)){snd:::sys_abort_NoArg(mtxName)}
  if(!is.data.frame(mtx)){snd:::sys_abort_WrongClass(x = mtx, class = c("data.frame", "tibble"))}

  #1. Grab keys, sort it ####
  keys = snd:::grab_mtxKey(dataframe = mtx)
  if(length(keys) != 0){
    key_priority = sapply(X = lapply(keys, FUN = snd:::classify_key),
                          FUN = snd:::key_priority, simplify = TRUE) %>%
      setNames(nm = keys) %>%
      sort(decreasing = TRUE)
    keys = names(key_priority)
  }

  #2. Grab factors ####
  factors = snd:::grab_mtxFactor(dataframe = mtx)

  #3. Sort everything, by key, factor, item ####
  return(invisible(dplyr::relocate(.data = mtx,
                                   dplyr::all_of(keys),
                                   dplyr::all_of(factors))))
}
