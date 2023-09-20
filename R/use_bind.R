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
  ##Extract factors, datasets n names ####
  snd1_dataset = snd1[sapply(X = snd1, snd::is_snd_set)]
  snd2_dataset = snd2[sapply(X = snd2, snd::is_snd_set)]

  snd1_name = names(snd1_dataset)
  snd2_name = names(snd2_dataset)

  ##rename if names have overlaps####
  duplicated_names = c(snd1_name, snd2_name)
  duplicated_names = unique(duplicated_names[duplicated(duplicated_names)])
  snd3_name = c(ifelse(snd1_name %in% duplicated_names, paste0(snd1_name, ".1"), snd1_name),
                ifelse(snd2_name %in% duplicated_names, paste0(snd2_name, ".2"), snd2_name))

  ##bind into snd3 ####
  snd3 = c(snd1_dataset, snd2_dataset) %>%
    snd:::nameAs(name = snd3_name)
  return(invisible(snd3))
}
