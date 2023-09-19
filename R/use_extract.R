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
  if(is.null(dataset)){
    snd:::sys_warn(message = c("!" = "{.arg dataset} is NULL",
                               "i" = "All datasets within {.arg snd} will be extracted"))
    dataset = names(snd)
  }
  #Extract start ####
  ##Show warning of datasets that are not in snd ####
  name = names(snd)
  failed_dataset = !(dataset %in% name)
  failed_dataset_name = dataset[failed_dataset]
  if(sum(failed_dataset)){
    custom = stringr::str_flatten(paste0("{.mtx ", failed_dataset_name, "}"), collapse = ", ")
    snd:::sys_warn(c("!" = "{.arg dataset} failed",
                     "!" = "{.arg dataset} can not find the following in {.arg snd}:",
                     "i" = custom))
  }
  ##Return ####
  return(invisible(snd[names(snd) %in% dataset]))
}
