#' Title
#'
#' @param xlsxFile
#' @param sheet
#'
#' @return
#' @export
#'
#' @examples
read_xlsx = function(xlsxFile, sheet){
  #Internal functions ####
  read_workbook = function(X, workbook){
    return(openxlsx::readWorkbook(xlsxFile = workbook, sheet = X))
  }
  classify = function(X, class){
    class(X) = c(class, class(X))
    return(X)
  }

  #Check ####
  if(!hasArg(xlsxFile)){snd:::sys_abort_NoArg(xlsxFile)}
  ##Grab all available sheets ####
  ava_factor = snd:::grab_xlsxFactor(xlsxFile = xlsxFile)
  ava_item = snd:::grab_xlsxItem(xlsxFile = xlsxFile)
  ava_data = snd:::grab_xlsxData(xlsxFile = xlsxFile)
  if(!hasArg(sheet)){sheet = ava_data}
  ##Continue ####
  snd:::checkRW_xlsx(xlsxFile = xlsxFile, sheet = sheet)
  #Use structure the read in process ####
  use_data = sheet
  use_factor = ava_factor
  use_item = paste0("#item",
                    stringr::str_sub(use_data, start = 6L, end = -1L)) %>%
    ifelse(. %in% ava_item, .,
           ifelse("#item" %in% ava_item, "#item", "FAIL"))
  #Read in the workbook, get all available styles in the workbook n turn it into string ####
  workbook = openxlsx::loadWorkbook(xlsxFile = xlsxFile)
  newStyle = openxlsx::createStyle(numFmt = "TEXT")
  for(i in 1:length(openxlsx::getStyles(wb = workbook))){
    openxlsx::replaceStyle(workbook, i, newStyle = newStyle)
  }
  #Start reading in the data of the above use_ list and format all the classes accordingly ####
  data_data = lapply(X = use_data, FUN = read_workbook, workbook = workbook) %>%
    lapply(FUN = classify, class = "snd_data")
  data_factor = read_workbook(X = use_factor, workbook = workbook) %>%
    classify("snd_factor")
  data_item = lapply(X = use_item, FUN = read_workbook, workbook = workbook) %>%
    lapply(FUN = classify, class = "snd_item")

  #Format it accordingly ####
  for(i in 1:length(data_data)){data_data[[i]] = snd:::formatRI_matrix(mtx = data_data[[i]], mtxName = use_data[[i]])}
  for(i in 1:length(data_item)){data_item[[i]] = snd:::formatRI_matrix(mtx = data_item[[i]], mtxName = use_item[[i]])}
  data_factor = snd:::formatRI_matrix(mtx = data_factor, mtxName = use_factor)
  use_key_factor = snd::grab_mtxKey(data_factor)
  for(i in 1:length(data_data)){
    use_key_item = snd:::grab_mtxKey(data_item[[i]])
    for(k in use_key_item){
      data_data[[i]] = snd:::formatRI_key2mtx(key = k,
                                            formater = data_item[[i]],
                                            formatee = data_data[[i]],
                                            formateeName = use_data[[i]])
    }
    for(k in use_key_factor){
      data_data[[i]] = snd:::formatRI_key2mtx(key = k,
                                            formater = data_factor,
                                            formatee = data_data[[i]],
                                            formateeName = use_data[[i]])
    }
  }

  #Format as SND ####
  snd = vector("list", length = (length(x = data_data) + 1))
  snd[[1]] = data_factor
  for(i in 1:length(data_data)){
    snd_set = list(item = data_item[[i]],
                   data = data_data[[i]])
    class(snd_set) = "snd_set"
    snd[[i+1]] = snd_set
  }
  class(snd) = "snd"
  #Give them names ####
  names(snd) = c("factor", stringr::str_sub(use_data, start = 7, end = -1L))
  return(invisible(snd))
}
