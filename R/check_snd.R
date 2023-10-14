#' @title
#' Check the structure of SND
#'
#' @description
#' A SND has a standard, fixed structure, which includes the following.
#' * dataset 1
#' * dataset 2
#' * dataset 3
#' * ...
#' * OS
#'
#' which a dataset includes
#' * snd_factor (named `factor`)
#' * snd_item (named `item`)
#' * data (named `data`)
#'
#' This function checks the structure of the SND and display a warning/ abort if...
#' * There is no datasets (Part 1, warning only)
#' * Names are duplicated between datasets (Part 2)
#' * There is no factor (Part 3)
#' * There is no item (Part 4)
#' * There is no data (Part 5)
#' * There is no OS (Part 6)
#'
#' @param snd an SND
#'
#' @return a standard message (if applicable)
#' @keywords internal
#' @example
check_snd = function(snd){
  #0. Check input####
  if(rlang::is_missing(snd)){snd:::sys_abort_NoArg(snd)}
  if(!snd::is_snd(snd)){snd:::sys_abort_WrongClass(x = snd, class = "snd")}
  if(length(snd) <= 0){
    snd:::sys_abort(message = c("x" = "0-length object",
                                "i" = "The object provided in {.arg {arg}} has no information",
                                "i" = "Please include some data within the object provided."),
                    arg = rlang::caller_arg(snd))
  }

  #0. Grab the set ####
  set = snd[sapply(X = snd, FUN = snd::is_snd_set, simplify = TRUE, USE.NAMES = FALSE)]
  #1. no sets check ####
  if(length(set) <= 0){
    snd:::sys_warn(message = c("!" = "No datasets",
                               "i" = "There is no data within {.arg {arg}}"),
                   arg = rlang::caller_arg(snd))
  }

  #2. duplicated name check####
  names = names(set)
  test = duplicated(names)
  if(sum(test)){
    failed_sets = stringr::str_flatten(string = paste0("{.set ", unique(names[test]), "}"),
                                       collapse = ", ")
    snd:::sys_abort(message = c("x" = "Duplicated set names",
                                "!" = "Each dataset must have a unique name",
                                "!" = "Duplicated names in {.arg {arg}} include:",
                                "i" = failed_sets),
                    arg = rlang::caller_arg(snd))
  }

  #3. snd_factor check ####
  factor = sapply(X = set,
                  FUN = function(X){
                    factor = X[sapply(X = X, FUN = snd::is_snd_factor)]
                    return(length(factor))
                  }, simplify = TRUE, USE.NAMES = FALSE)
  test = (factor != 1)
  if(sum(test)){
    failed_sets = stringr::str_flatten(string = paste0("{.set ", names(set[test]), "}"),
                                       collapse = ", ")
    snd:::sys_abort(message = c("x" = "Lack/ Excess {.mtx factor} in {.arg {arg}}",
                                "!" = "{.mtx factor} descibes all the factors in {.mtx data}",
                                "!" = "There must be only 1 {.mtx factor} in each dataset",
                                "!" = "Datasets with {.mtx factor} abnormality include:",
                                "i" = failed_sets),
                    arg = rlang::caller_arg(snd))
  }

  #4. snd_item check ####
  item = sapply(X = set,
                FUN = function(X){
                  item = X[sapply(X = X, FUN = snd::is_snd_item)]
                  return(length(item))
                }, simplify = TRUE, USE.NAMES = FALSE)
  test = (item != 1)
  if(sum(test)){
    failed_sets = stringr::str_flatten(string = paste0("{.set ", names(set[test]), "}"),
                                       collapse = ", ")
    snd:::sys_abort(message = c("x" = "Lack/ Excess {.mtx item} in {.arg {arg}}",
                                "!" = "{.mtx item} descibes all the items in {.mtx data}",
                                "!" = "There must be only 1 {.mtx item} in each dataset",
                                "!" = "Datasets with {.mtx item} abnormality include:",
                                "i" = failed_sets),
                    arg = rlang::caller_arg(snd))
  }

  #5. snd_data check ####
  data = sapply(X = set,
                FUN = function(X){
                  data = X[sapply(X = X, FUN = snd::is_snd_data)]
                  return(length(data))
                }, simplify = TRUE, USE.NAMES = FALSE)
  test = (data != 1)
  if(sum(test)){
    failed_sets = stringr::str_flatten(string = paste0("{.set ", names(set[test]), "}"),
                                       collapse = ", ")
    snd:::sys_abort(message = c("x" = "Lack/ Excess {.mtx data} in {.arg {arg}}",
                                "!" = "{.mtx data} descibes all the data in the dataset",
                                "!" = "There must be only 1 {.mtx data} in each dataset",
                                "!" = "Datasets with {.mtx item} abnormality include:",
                                "i" = failed_sets),
                    arg = rlang::caller_arg(snd))
  }
  #6. snd_os check ####
  OS = sapply(X = snd,
              FUN = snd::is_snd_os,
              simplify = TRUE, USE.NAMES = FALSE)
  if(sum(OS) != 1){
    snd:::sys_abort(message = c("x" = "Lack/ Excess {.mtx OS}",
                                "!" = "{.mtx OS} descibes how the snd/ seal functions should operate",
                                "!" = "There should be only 1 {.mtx os} in snd"),
                    x = snd)
  }
}
