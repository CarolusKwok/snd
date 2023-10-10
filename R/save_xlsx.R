#' Title
#'
#' @param snd
#' @param xlsxFile
#'
#' @return
#' @export
#'
#' @examples
save_xlsx = function(snd, xlsxFile){
  snd = snd[sapply(X = snd, FUN = snd::is_snd_set)]
  names = names(snd)

  #Format snd's data
  for(i in 1:length(snd)){
    index_data = match(x = TRUE, table = sapply(X = snd[[i]], FUN = snd::is_snd_data))
    keys = snd::grab_mtxKey(snd[[i]][[index_data]])
    mtxName = names[[i]]
    for(k in keys){
      snd[[i]][[index_data]] = snd:::formatWO_key(key = k, mtx = snd[[i]][[index_data]], mtxName = paste0("#data_", mtxName))
    }
  }

  #Format snd's data by snd's factor
  for(i in 1:length(snd)){
    index_data = match(x = TRUE, table = sapply(X = snd[[i]], FUN = snd::is_snd_data))
    index_factor = match(x = TRUE, table = sapply(X = snd[[i]], FUN = snd::is_snd_factor))
    keys = snd::grab_mtxKey(snd[[i]][[index_factor]])
    mtxName = names[[i]]
    for(k in keys){
      formatted = snd:::formatWO_key2mtx(key = k, formater = snd[[i]][[index_factor]], formatee = snd[[i]][[index_data]],
                                         formaterName = paste0("#factor_", mtxName), formateeName = paste0("#data_", mtxName))
      snd[[i]][[index_factor]] = formatted$formater
      snd[[i]][[index_data]] = formatted$formatee
    }
  }

  #Format snd's data by snd's item
  for(i in 1:length(snd)){
    index_data = match(x = TRUE, table = sapply(X = snd[[i]], FUN = snd::is_snd_data))
    index_item = match(x = TRUE, table = sapply(X = snd[[i]], FUN = snd::is_snd_item))
    keys = snd::grab_mtxKey(snd[[i]][[index_item]])
    mtxName = names[[i]]
    for(k in keys){
      formatted = snd:::formatWO_key2mtx(key = k, formater = snd[[i]][[index_item]], formatee = snd[[i]][[index_data]],
                                         formaterName = paste0("#item_", mtxName), formateeName = paste0("#data_", mtxName))
      snd[[i]][[index_item]] = formatted$formater
      snd[[i]][[index_data]] = formatted$formatee
    }
  }

  #Start listing
  list_data = lapply(X = snd,
                     FUN = function(X){
                       index = match(x = TRUE, table = sapply(X = X, FUN = snd::is_snd_data))
                       return(X[[index]])
                     }) %>%
    setNames(nm = paste0("#data_", names(.)))

  list_factor = lapply(X = snd,
                       FUN = function(X){
                         index = match(x = TRUE, table = sapply(X = X, FUN = snd::is_snd_factor))
                         return(X[[index]])
                       }) %>%
    setNames(nm = paste0("#factor_", names(.)))

  list_item = lapply(X = snd,
                     FUN = function(X){
                       index = match(x = TRUE, table = sapply(X = X, FUN = snd::is_snd_item))
                       return(X[[index]])
                     }) %>%
    setNames(nm = paste0("#item_", names(.)))

  list = append(x = list_factor, values = list_item) %>%
    append(values = list_data)
  openxlsx::write.xlsx(x = list, file = xlsxFile)
}
