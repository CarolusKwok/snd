#' Binds 2 SNDs together
#'
#' @description
#' For an SND to maintain it's structure, the data dataframe can not be simply combined! This function combines the datasets while still maintaining the SND structure. It also checks if the bind is reasonable.
#'
#' If there are datasets of the same name, it will be denoted with a suffix ".1" and ".2". There should not be datasets with the same name.
#'
#' @param snd1 The first SND
#' @param snd2 The second SND
#'
#' @return A SND
#' @export
#'
#' @examples bind(snd1, snd2)
bind = function(snd1, snd2){
  #Checks ####
  snd:::check_snd(snd1)
  snd:::check_snd(snd2)

  #Start ####
  ##Get set names ####
  snd1_setName = names(snd1)[sapply(snd1, FUN = snd::is_snd_set)]
  snd2_setName = names(snd2)[sapply(snd2, FUN = snd::is_snd_set)]

  ##Find duplicate names, paste suffix if set names are duplicated####
  dup_setName = c(snd1_setName, snd2_setName)
  dup_setName = unique(dup_setName[duplicated(dup_setName)])
  snd1_setName = ifelse(snd1_setName %in% dup_setName, paste0(snd1_setName, ".1"), snd1_setName)
  snd2_setName = ifelse(snd2_setName %in% dup_setName, paste0(snd2_setName, ".2"), snd2_setName)
  ##Find duplicated names, paste additional suffix if set names duplicated ####
  snd3_setName = make.unique(c(snd1_setName, snd2_setName))
  snd1_setName = snd3_setName[1:length(snd1_setName)]
  snd2_setName = snd3_setName[(length(snd1_setName)+1):length(snd3_setName)]
  ##Rename snd1 and snd2 base on setName ####
  snd1 = snd::rename(snd1, snd1_setName)
  snd2 = snd::rename(snd2, snd2_setName)

  ##Grab OS and bind ####
  snd1_OS = unclass(snd1[sapply(X = snd1, FUN = snd::is_snd_os)][[1]])
  snd2_OS = unclass(snd2[sapply(X = snd2, FUN = snd::is_snd_os)][[1]])

  snd1_OSkey = names(snd1_OS)
  snd2_OSkey = names(snd2_OS)
  snd3_OSkey = unique(c(snd1_OSkey, snd2_OSkey))

  snd3_OS = setNames(object = lapply(X = snd3_OSkey, FUN = function(X){return(NULL)}),
                     nm = snd3_OSkey)

  for(i in 1:length(snd3_OS)){
    sel_key = snd3_OSkey[[i]]
    snd1_OSindex = match(x = sel_key, table = snd1_OSkey)
    snd2_OSindex = match(x = sel_key, table = snd2_OSkey)

    if(is.na(snd1_OSindex)){
      snd3_OS[[i]] = snd2_OS[[snd2_OSindex]]
    } else if(is.na(snd2_OSindex)){
      snd3_OS[[i]] = snd1_OS[[snd1_OSindex]]
    } else {
      snd3_OS[[i]] = c(snd1_OS[[snd1_OSindex]], snd2_OS[[snd2_OSindex]])
    }
  }
  snd3_OS = snd:::classify_os(snd3_OS)

  ##Blend everything n form snd3 ####
  snd3 = append(x = snd1[sapply(snd1, FUN = snd::is_snd_set)],
                values = snd2[sapply(snd2, FUN = snd::is_snd_set)]) %>%
    append(values = list(snd3_OS)) %>%
    setNames(nm = c(snd3_setName, "OS")) %>%
    snd:::classify_snd(.)
  return(invisible(snd3))
}
