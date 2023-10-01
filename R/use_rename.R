#' Title
#'
#' @param snd
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rename = function(snd, ...){
  #Check
  snd:::check_snd(snd)

  ##Find the OG names###
  is_sndset = sapply(X = snd, FUN = snd::is_snd_set)
  OGname = names(snd)[is_sndset]

  #Show warning if ... is empty
  dots = list(...)
  if(length(dots) == 0){
    snd:::sys_warn(message = c("!" = "{.arg ...} is empty",
                               "!" = "No dataset in {.arg snd} will be renamed"))
    return(invisible(snd))
  }
  #Find the appropriate method
  dots_name = names(dots)
  if(is.null(dots_name) | sum(dots_name == "")){
    method = "unnamed"
    dots = unlist(unname(dots))
  } else {
    method = "named"
  }

  #Start
  if(method == "unnamed"){ #UNnamed method ####
    if(length(dots) < length(OGname)){
      snd:::sys_warn(message = c("!" = "Insufficient names in {.arg ...}",
                                 "i" = "There are {length(OGname)} datasets in {.arg snd}",
                                 "!" = "You only provide {length(dots)}",
                                 "i" = "Only the first {length(dots)} datasets will be renamed",
                                 "i" = "The remaining datasets will maintain its original name"),
                     OGname = OGname, dots = dots)
      modNames = c(dots, OGname[(length(dots)+1):length(OGname)])
    } else if(length(dots) > length(OGname)){
      snd:::sys_warn(message = c("!" = "Overflowing names in {.arg ...}",
                                 "i" = "There are {length(OGname)} datasets in {.arg snd}",
                                 "!" = "You only provide {length(dots)}",
                                 "i" = "The overflowed names will be ignored"),
                     OGname = OGname, dots = dots)
      modNames = dots[1:length(OGname)]
    } else {
      modNames = dots
    }
  } else if (method == "named"){  #named method ####
    modNames = OGname
    for(i in 1:length(dots)){
      index = match(x = dots[[i]], table = OGname, nomatch = NA)
      if(is.na(index)){
        next
      } else {
        modNames[[index]] = names(dots[i])
      }
    }
  }
  modNames = snd:::getUniqueNames(modNames)

  ##Modifying OS defaultMod ####
  defaultMod = snd:::OSgrab(snd, OSkey = "defaultMod")
  test = (OGname != modNames)
  defaultMod = sapply(X = defaultMod,
                      FUN = function(X, test_OGname, test_modNames){
                        if(X %in% test_OGname){
                          index = match(x = X, table = test_OGname)
                          return(test_modNames[[index]])
                        } else {
                            return(X)
                        }
                      },
                      test_OGname = OGname[test], test_modNames = modNames[test], USE.NAMES = FALSE, simplify = TRUE)
  snd = snd:::OSmodify(snd, OSkey = "defaultMod", value = defaultMod)

  ##Actually start renameing ####
  names = names(snd)
  names = sapply(X = 1:length(names),
                 FUN = function(X, is_sndset, names, modNames){
                   if(is_sndset[[X]]){
                     return(modNames[sum(is_sndset[1:X])])
                   }
                   else{return(names[[X]])}},
                 is_sndset = is_sndset, names = names, modNames = modNames)
  return(invisible(setNames(object = snd, nm = names)))
}
