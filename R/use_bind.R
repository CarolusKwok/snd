#' Title
#'
#' @param snd1
#' @param snd2
#'
#' @return
#' @export
#'
#' @examples
bind = function(snd1, snd2){
  #Checks ####
  snd:::check_snd(snd1)
  snd:::check_snd(snd2)

  #Start binding ####
  ##Extract factors, datasets n names ####
  snd1_factor = snd1[sapply(X = snd1, snd::is_snd_factor)]
  snd2_factor = snd2[sapply(X = snd2, snd::is_snd_factor)]

  snd1_dataset = snd1[sapply(X = snd1, snd::is_snd_set)]
  snd2_dataset = snd2[sapply(X = snd2, snd::is_snd_set)]

  snd1_name = names(snd1_dataset)
  snd2_name = names(snd2_dataset)

  ##rename if names have overlaps####
  duplicated_names = c(snd1_name, snd2_name)
  duplicated_names = unique(duplicated_names[duplicated(duplicated_names)])
  snd3_name = c(ifelse(snd1_name %in% duplicated_names, paste0(snd1_name, ".1"), snd1_name),
                ifelse(snd2_name %in% duplicated_names, paste0(snd2_name, ".2"), snd2_name))

  ##bind the snd3_dataset ####
  snd3_dataset = c(snd1_dataset, snd2_dataset)

  ##Check if the factors are combindable ####
  snd3_factor = dplyr::bind_rows(unname(snd1_factor), unname(snd2_factor)) %>%
    dplyr::distinct()
  snd:::checkRW_matrix.snd_factor(mtx = snd3_factor, mtxName = "factor")

  #snd!
  snd3 = append(x = list(snd3_factor), values = snd3_dataset)
  names(snd3) = c("factor", snd3_name)
  return(snd3)
}
