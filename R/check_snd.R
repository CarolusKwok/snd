#' Title
#'
#' @param snd
#'
#' @return
#' @keywords internal
check_snd = function(snd){
  if(rlang::is_missing(snd)){snd:::sys_abort_NoArg(snd)}
  if(!snd::is_snd(snd)){snd:::sys_abort_WrongClass(x = snd, class = "snd")}
  #SND must contain 1 snd_factor and x sets of snd_set
  #snd set must contain only 1 snd_data n 1 snd_item
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
    print("holy fuck3")
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
