#' Extracting a dataset in SND
#'
#' For an SND to maintain it's structure, the data dataframe can not be read alone! This function extracts the dataset while still maintaining the SND structure (i.e. also extracting the factor and item matrixes).
#' People do not need to call the names of the factor matrix or item matrix while using this function, just type the name(s) of the dataset (e.g. `c("reference_1", "treatment_1")`) in the argument `dataset`.
#' By default (`NULL`), all datasets will be extracted (i.e. returning the original SND). If the name(s) in `dataset` is not found in the SND, the name will be ignored, and the extraction will continue.
#'
#' @param snd the SND object.
#' @param dataset the names of datasets to be extracted, in character.
#'
#' @return An SND
#' @export
#'
#' @examples extract(snd, c("month1", "month3"))
extract = function(snd, dataset = NULL){
  snd:::check_snd(snd)

  #Find the things to extract ####
  index = tibble::tibble(name = names(snd),
                         index_factor = unname(sapply(X = snd, FUN = snd::is_snd_factor)),
                         index_set    = unname(sapply(X = snd, FUN = snd::is_snd_set)))
  if(sum(is.null(dataset))){
    index = dplyr::mutate(.data = index,
                          in_selection = TRUE)
  } else {
    index = dplyr::mutate(.data = index,
                          in_selection = name %in% dataset)
  }
  index = dplyr::mutate(.data = index,
                        extracted = ifelse(index_factor,
                                           TRUE,
                                           index_set & in_selection))
  #Extract ####
  return(invisible(snd[index$extracted]))
}
