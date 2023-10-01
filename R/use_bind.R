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

  #Start binding ####
  ## Find all set names in snd1 and snd2, combine to find snd3's names####
  grabSNDSetNames = function(snd){
    sapply(snd1, FUN = snd::is_snd_set) %>%
      .[.] %>%
      names %>%
      return
  }

  snd1_originalnames = grabSNDSetNames(snd = snd1)
  snd2_originalnames = grabSNDSetNames(snd = snd2)
  ## Find duplicated names ####
  snd3_dupnames = c(snd1_originalnames, snd2_originalnames) %>% unique(.[duplicated(.)])
  ## Modify duplicated names n Modify names again by suffix if duplicated ####
  snd1_names = ifelse(snd1_originalnames %in% snd3_dupnames, paste0(snd1_originalnames, ".1"), snd1_originalnames)
  snd2_names = ifelse(snd2_originalnames %in% snd3_dupnames, paste0(snd2_originalnames, ".2"), snd2_originalnames)
  snd3_names = snd:::getUniqueNames(name = c(snd1_names, snd2_names))

  ##Get the modded names for each SND, rename####
  snd1 = snd::rename(snd1, snd3_names[1:length(snd1_names)])
  snd2 = snd::rename(snd2, snd3_names[(length(snd1_names)+1):length(snd3_names)])

  ##Grab the OS of snd1 n snd2 ####
  snd1_OS = unclass(snd1[sapply(X = snd1, FUN = snd::is_snd_os)][[1]])
  snd2_OS = unclass(snd2[sapply(X = snd2, FUN = snd::is_snd_os)][[1]])

  l = list(snd1_OS, snd2_OS)
  keys = unique(unlist(lapply(l, names)))
  snd3_OS = setNames(do.call(mapply, c(FUN=c, lapply(l, `[`, keys))), keys) %>%
    snd:::classify_os(x = .)
  ## Bind the snds together
  append(x = snd::grab_sndset(snd1),
         values = snd::grab_sndset(snd2)) %>%
    append(values = list(snd3_OS)) %>%
    setNames(nm = c(snd3_names, "OS")) %>%
    snd:::classify_snd(x = .) %>%
    invisible() %>%
    return(.)
}
