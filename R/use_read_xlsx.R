#' Read in an SND with xlsx
#'
#' @description
#' SND data can be created and formatted via external programs, e.g. Microsoft Excel, Google Sheets, and LibreOffice Calc. This function below reads in the SND data from a `.xlsx` file into the R environment. the datasets to read in can be specified using the `sheet` argument, and only the data matrix needs to be called. If a sheet name in `sheet` is unavailable, the function will abort.
#'
#' The matrix contained in xlsx are seperated by different xlsx sheets, and specified as part of the SND with `"#"`, followed by the type of matrix it contains. The currently supported spreadsheet names and matrix includes the following:
#' - #factor
#' - #item
#' - #data
#'
#' To specify the name of the matrix, a suffix `"_"` followed by it's name can be used. This function is only supported in #item and #data now, e.g.
#' - #item_ecology, #data_ecology (the name of the matrix are "ecology")
#' - #data_environment (the name of the data matrix is "environment")
#'
#' If an item matrix is not named, it will be set as the "general" matrix. This matrix will be used if no "specific" matrix is used for the data matrix. For more information, please read the Notion documents.
#'
#' @param xlsxFile Directory of the .xlsx file
#' @param sheet Name of sheets to be read it, in `character`. Note that only sheets containing data needs to be specified. By default (`NULL`), all available data will be read.
#' @return SND
#' @export
#' @examples read_xlsx(xlsxFile, sheet = NULL)
read_xlsx = function(xlsxFile, sheet = NULL){
  #Internal functions ####
  read_workbook = function(X, workbook){
    return(openxlsx::readWorkbook(xlsxFile = workbook, sheet = X))
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
  usename_data = sheet
  usename_item = paste0("#item",
                        stringr::str_sub(usename_data, start = 6L, end = -1L)) %>%
    ifelse(. %in% ava_item, .,
           ifelse("#item" %in% ava_item, "#item", "FAIL"))
  usename_factor = paste0("#factor",
                          stringr::str_sub(usename_data, start = 6L, end = -1L)) %>%
    ifelse(. %in% ava_factor, .,
           ifelse("#factor" %in% ava_factor, "#factor", "FAIL"))
  #Read in the workbook, get all available styles in the workbook n turn it into string ####
  workbook = openxlsx::loadWorkbook(xlsxFile = xlsxFile)
  newStyle = openxlsx::createStyle(numFmt = "TEXT")
  for(i in 1:length(openxlsx::getStyles(wb = workbook))){
    openxlsx::replaceStyle(workbook, i, newStyle = newStyle)
  }
  #Find the unique matrix name for items to prevent reading in item multiple times
  usename_item_unique = unique(usename_item)
  names(usename_item_unique) = usename_item_unique

  usename_factor_unique = unique(usename_factor)
  names(usename_factor_unique) = usename_factor_unique

  #Start reading in the data of the above use_ list and format all the classes accordingly ####
  data_data = lapply(X = lapply(X = usename_data, FUN = read_workbook, workbook = workbook),
                     FUN = snd:::classify, class = "snd_data")
  data_item_unique = lapply(X = lapply(X = usename_item_unique, FUN = read_workbook, workbook = workbook),
                            FUN = snd:::classify, class = "snd_item")
  data_factor_unique = lapply(X = lapply(X = usename_factor_unique, FUN = read_workbook, workbook = workbook),
                              FUN = snd:::classify, class = "snd_factor")

  #Format itself accordingly ####
  data_data = mapply(FUN = snd:::formatRI_matrix, mtx = data_data, mtxName = usename_data, SIMPLIFY = FALSE)
  data_item_unique = mapply(FUN = snd:::formatRI_matrix, mtx = data_item_unique, mtxName = usename_item_unique, SIMPLIFY = FALSE)
  data_factor_unique = mapply(FUN = snd:::formatRI_matrix, mtx = data_factor_unique, mtxName = usename_factor_unique, SIMPLIFY = FALSE)

  #Get data_item and data_factor instead of data_item_unique and data_factor_unique ####
  data_item = usename_item
  data_item = lapply(X = data_item,
                     FUN = function(X, data_item_unique){return(data_item_unique[[match(X, table = names(data_item_unique))]])},
                     data_item_unique = data_item_unique)

  data_factor = usename_factor
  data_factor = lapply(X = data_factor,
                       FUN = function(X, data_factor_unique){return(data_factor_unique[[match(X, table = names(data_factor_unique))]])},
                       data_factor_unique = data_factor_unique)

  #Format based on factor and item ####
  data_data = mapply(FUN = function(data_data, data_item, usename_data){
    use_key_item = snd::grab_mtxKey(data_item)
    for(k in use_key_item){data_data = snd:::formatRI_key2mtx(key = k,
                                                              formater = data_item,
                                                              formatee = data_data,
                                                              formateeName = usename_data)}
    return(invisible(data_data))},
    data_data = data_data, data_item = data_item, usename_data = usename_data, SIMPLIFY = FALSE)

  data_data = mapply(FUN = function(data_data, data_factor, usename_data){
    use_key_factor = snd::grab_mtxKey(data_factor)
    for(k in use_key_factor){data_data = snd:::formatRI_key2mtx(key = k,
                                                                formater = data_factor,
                                                                formatee = data_data,
                                                                formateeName = usename_data)}
    return(invisible(data_data))},
    data_data = data_data, data_factor = data_factor, usename_data = usename_data, SIMPLIFY = FALSE)

  #Package as SND ####
  snd = mapply(FUN = function(data_factor, data_item, data_data){
    snd_set = list(factor = data_factor, item = data_item, data = data_data)
    class(snd_set) = "snd_set"
    return(snd_set)},
    data_factor = data_factor, data_item = data_item, data_data = data_data, SIMPLIFY = FALSE)
  class(snd) = "snd"

  #Give them names ####
  names(snd) = stringr::str_sub(usename_data, start = 7, end = -1L)

  #Return ####
  return(invisible(snd))
}
