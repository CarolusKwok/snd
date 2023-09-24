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

  #Extract start ####
  name_set = sapply(X = snd,
                    FUN = snd::is_snd_set, simplify = TRUE, USE.NAMES = TRUE) %>%
    .[.] %>%
    names

  ##show warning if dataset is NULL ####
  if(is.null(dataset)){
    snd:::sys_warn(message = c("!" = "{.arg dataset} is NULL",
                               "i" = "All datasets within {.arg snd} will be extracted"))
    dataset = name_set
  }

  ##Show warning of datasets that are not in snd ####
  fail = !(dataset %in% name_set)
  if(sum(fail)){
    custom = stringr::str_flatten(paste0("{.mtx ", dataset[fail], "}"), collapse = ", ")
    snd:::sys_warn(c("!" = "{.arg dataset} failed",
                     "!" = "{.arg dataset} can not find the following in {.arg snd}:",
                     "i" = custom))
  }

  ##Extract neccessary datasets n OS####
  test  = ((sapply(X = snd, FUN = snd::is_snd_set) &
              (names(snd) %in% dataset)) |
             sapply(X = snd, FUN = snd::is_snd_os))
  extracted_snd = snd:::classify_snd(x = (snd[test]))

  ##Modify the extracted_snd's defaultMod ####
  defaultMod = snd:::OSgrab(snd, OSkey = "defaultMod") %>% .[. %in% dataset]
  snd:::OSmodify(extracted_snd, OSkey = "defaultMod", value = defaultMod) %>%
    return(invisible(.))
}
