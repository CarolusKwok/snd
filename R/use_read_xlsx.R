#' Title
#'
#' @param xlsxFile
#' @param sheet
#'
#' @return
#' @export
#'
#' @examples
read_xlsx = function(xlsxFile, sheet = NULL){
  #Internal functions ####
  read_workbook = function(X, workbook){
    return(openxlsx::readWorkbook(xlsxFile = workbook, sheet = X))
  }
  classify = function(X, class){
    class(X) = c(class, class(X))
    return(X)
  }

  #Check if sheets are available####
  if(!hasArg(xlsxFile)){snd:::sys_abort_NoArg(xlsxFile)}
  snd:::checkRW_xlsx(xlsxFile = xlsxFile, sheet = sheet)

  ##Grab all available sheets ####
  ava_factor = snd:::grab_xlsxFactor(xlsxFile = xlsxFile)
  ava_item = snd:::grab_xlsxItem(xlsxFile = xlsxFile)
  ava_data = snd:::grab_xlsxData(xlsxFile = xlsxFile)
  if(is.null(sheet)){sheet = ava_data}
  #structure the data as final product, using names only####
  usename_factor = ava_factor
  usename_data = sheet
  usename_item = paste0("#item",
                        stringr::str_sub(usename_data, start = 6L, end = -1L)) %>%
    ifelse(. %in% ava_item, .,
           ifelse("#item" %in% ava_item, "#item", "FAIL"))
  #Read in the workbook, get all available styles in the workbook n turn it into string ####
  workbook = openxlsx::loadWorkbook(xlsxFile = xlsxFile)
  newStyle = openxlsx::createStyle(numFmt = "TEXT")
  for(i in 1:length(openxlsx::getStyles(wb = workbook))){
    openxlsx::replaceStyle(workbook, i, newStyle = newStyle)
  }
  #Find the unique matrix name for items to prevent reading in item multiple times
  usename_item_unique = unique(usename_item)
  names(usename_item_unique) = usename_item_unique


  #Start reading in the data of the above use_ list and format all the classes accordingly ####
  data_data = lapply(X = lapply(X = usename_data, FUN = read_workbook, workbook = workbook),
                     FUN = classify, class = "snd_data")
  data_item_unique = lapply(X = lapply(X = usename_item_unique, FUN = read_workbook, workbook = workbook),
                            FUN = classify, class = "snd_item")
  data_factor = classify(X = read_workbook(X = usename_factor, workbook = workbook), class = "snd_factor")

  #Format itself accordingly ####
  data_data = mapply(FUN = snd:::formatRI_matrix, mtx = data_data, mtxName = usename_data, SIMPLIFY = FALSE)
  data_item_unique = mapply(FUN = snd:::formatRI_matrix, mtx = data_item_unique, mtxName = usename_item_unique, SIMPLIFY = FALSE)
  data_factor = snd:::formatRI_matrix(mtx = data_factor, mtxName = usename_factor)

  #Get data_item instead of data_item_unique ####
  data_item = usename_item
  data_item = lapply(X = data_item,
                     FUN = function(X, data_item_unique){return(data_item_unique[[match(X, table = names(data_item_unique))]])},
                     data_item_unique = data_item_unique)

  #Format based on factor and item ####
  data_data = mapply(FUN = function(data_data, data_item, usename_data){
    use_key_item = snd::grab_mtxKey(data_item)
    for(k in use_key_item){data_data = snd:::formatRI_key2mtx(key = k,
                                                              formater = data_item,
                                                              formatee = data_data,
                                                              formateeName = usename_data)}
    return(invisible(data_data))},
    data_data = data_data, data_item = data_item, usename_data = usename_data, SIMPLIFY = FALSE)

  use_key_factor = snd::grab_mtxKey(data_factor)
  for(i in 1:length(data_data)){
    for(k in use_key_factor){
      data_data[[i]] = snd:::formatRI_key2mtx(key = k,
                                              formater = data_factor,
                                              formatee = data_data[[i]],
                                              formateeName = usename_data[[i]])
    }
  }

  #Package as SND ####
  snd_sets = mapply(FUN = function(data_item, data_data){
    snd_set = list(item = data_item, data = data_data)
    class(snd_set) = "snd_set"
    return(snd_set)},
    data_item = data_item, data_data = data_data, SIMPLIFY = FALSE)
  snd = append(list(factor = data_factor), values = snd_sets)
  class(snd) = "snd"

  #Give them names ####
  names(snd) = c("factor", stringr::str_sub(usename_data, start = 7, end = -1L))

  #Return ####
  return(invisible(snd))
}
