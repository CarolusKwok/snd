#' Check the structure of SND
#'
#' A SND has a standard, fixed structure, which includes the following.
#' * factor
#' * dataset 1
#' * dataset 2
#' * dataset 3
#' * ...
#'
#' which a dataset includes
#' * item
#' * data
#'
#' This function checks the structure of the SND and display a warning/ abort if...
#' * There is no factor matrix (Part 1)
#' * There is no datasets (Part 2, warning only)
#' * There is no item (Part 3)
#' * There is no data (Part 4)
#'
#' @param snd an SND
#'
#' @return a standard abort message (if applicable)
#' @keywords internal
check_snd = function(snd){
  if(rlang::is_missing(snd)){snd:::sys_abort_NoArg(snd)}
  if(!snd::is_snd(snd)){snd:::sys_abort_WrongClass(x = snd, class = "snd")}
  #Part 1 snd_factor check ####
  flag = lapply(X = snd, FUN = snd::is_snd_factor)
  if(sum(unlist(flag)) != 1){
    snd:::sys_abort(message = c("x" = "Lacking factors",
                                "!" = "{.cls snd} must contain only 1 {.mtx factor}",
                                "i" = "Please check if {.arg {arg}} is corrupted"),
                    arg = rlang::caller_arg(snd))
  }
  #Grab the set
  set = snd[unlist(lapply(snd, FUN = snd::is_snd_set))]
  #Part 2 display warning if there is no set ####
  if(length(set) <= 0){
    snd:::sys_warn(message = c("!" = "No datasets",
                               "i" = "There is no data within {.arg {arg}}",
                               "i" = "Please check if the {.arg {arg}} is corrupted"),
                   x = snd)
  }
  #Part 3 snd_item check ####
  item = lapply(X = lapply(X = set,
                           FUN = function(X){X[unlist(lapply(X, FUN = snd::is_snd_item))]}),
                FUN = length)
  if(sum(unlist(item != 1))){
    snd:::sys_abort(message = c("x" = "Lack {.mtx item} for datasets",
                                "!" = "{.mtx item} descibes all the items in datasets",
                                "!" = "There must be only 1 {.mtx item} in each dataset",
                                "i" = "Please check if {.arg {arg}} is corrupted"),
                    x = snd)
  }
  #Part 4 snd_data check ####
  data = lapply(lapply(X = set,
                       FUN = function(X){X[unlist(lapply(X, FUN = snd::is_snd_data))]}),
                FUN = length)
  if(sum(unlist(data != 1))){
    snd:::sys_abort(message = c("x" = "Lack {.mtx data} for datasets",
                                "!" = "{.mtx data} descibes all the data in datasets",
                                "!" = "There must be only 1 {.mtx data} in each dataset",
                                "i" = "Please check if {.arg {arg}} is corrupted"),
                    x = snd)
  }
}
