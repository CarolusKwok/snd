#' Title
#'
#' @param snd
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
copy = function(snd, dataset = NULL){
  snd:::check_snd(snd)
  if(is.null(dataset)){
    snd:::sys_warn(message = c("!" = "{.arg dataset} is {.arg NULL}",
                               "i" = "{.arg dataset} can't be {.arg NULL}",
                               "i" = "This is to prevent copying every single dataset in {.arg snd}",
                               "!" = "Returning the original {.arg snd} now",
                               "i" = "Please add {.arg dataset}"))
    return(invisible(snd))
  }
  #Remove any names that's not in snd ####
  snd_setName = names(snd)[sapply(X = snd, FUN = snd::is_snd_set)]
  if(sum(!(dataset %in% snd_setName))){
    dataset = dataset[dataset %in% snd_setName]
    if(length(dataset) != 0){
      snd:::sys_warn(message = c("!" = "{.arg dataset} contains names not in {.arg snd}",
                                 "!" = "This will cause errors in the copying process",
                                 "!" = "Incorrect names from {.arg dataset} are now removed",
                                 "i" = "Current {.arg dataset} names:",
                                 "i" = "{dataset}"),
                     dataset = dataset)
    } else {
      snd:::sys_warn(message = c("!" = "{.arg dataset} contains names not in {.arg snd}",
                                 "!" = "This will cause errors in the copying process",
                                 "!" = "Incorrect names from {.arg dataset} are now removed",
                                 "i" = "Current {.arg dataset} names contain no usable dataset",
                                 "i" = "Returning the original {.arg snd} now",
                                 "i" = "Please add usable names in {.arg dataset}"))
      return(invisible(snd))
    }
  }

  #Make extracted snd ####
  snd2 = snd::split(snd, dataset = dataset)
  snd2_setName = paste0(names(snd2)[sapply(X = snd2, FUN = snd::is_snd_set)], "(copy)")
  sndBind_setName = make.unique(names = c(names(snd)[sapply(X = snd, FUN = snd::is_snd_set)],
                                          snd2_setName))
  snd2_setName = sndBind_setName[(length(sndBind_setName)-length(snd2_setName)+1):length(sndBind_setName)]
  snd2 = snd::rename(snd2, snd2_setName)

  snd2_OSkey = names(snd2[sapply(snd2, snd::is_snd_os)][[1]])
  for(i in snd2_OSkey){
    snd2 = snd:::OSmodify(snd = snd2,
                          OSkey = i,
                          value = NULL)
  }

  #Bind it ####
  snd3 = snd::bind(snd1 = snd,
                   snd2 = snd2)
  return(invisible(snd3))
}
