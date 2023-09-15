#' Title
#'
#' @param snd
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
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
