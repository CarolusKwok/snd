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
  #Check if sheets are available####
  if(rlang::is_missing(xlsxFile)){snd:::sys_abort_NoArg(xlsxFile)}
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

  #Find the unique matrix name for items to prevent reading in item multiple times
  usename_item_unique = unique(usename_item) %>%
    setNames(object = ., nm = .)

  usename_factor_unique = unique(usename_factor) %>%
    setNames(object = ., nm = .)

  #Start reading in the data of the above use_ list and format all the classes accordingly ####
  workbook = openxlsx::loadWorkbook(xlsxFile = xlsxFile)
  newStyle = openxlsx::createStyle(numFmt = "TEXT")
  lapply(X = seq_along(openxlsx::getStyles(wb = workbook)),
         FUN = openxlsx::replaceStyle, wb = workbook, newStyle = newStyle)
  data_data = lapply(X = usename_data,
                     FUN = function(X, workbook){
                       return(snd:::classify_data(openxlsx::readWorkbook(xlsxFile = workbook, sheet = X)))
                     }, workbook = workbook)
  data_item_unique = lapply(X = usename_item_unique,
                            FUN = function(X, workbook){
                              return(snd:::classify_item(x = openxlsx::readWorkbook(xlsxFile = workbook, sheet = X)))
                            }, workbook = workbook)
  data_factor_unique = lapply(X = usename_factor_unique,
                              FUN = function(X, workbook){
                                return(snd:::classify_factor(x = openxlsx::readWorkbook(xlsxFile = workbook, sheet = X)))
                              }, workbook = workbook)

  #Format itself accordingly ####
  data_data = mapply(FUN = snd:::formatRI_matrix, mtx = data_data, mtxName = usename_data, SIMPLIFY = FALSE)
  data_item_unique = mapply(FUN = snd:::formatRI_matrix, mtx = data_item_unique, mtxName = usename_item_unique, SIMPLIFY = FALSE)
  data_factor_unique = mapply(FUN = snd:::formatRI_matrix, mtx = data_factor_unique, mtxName = usename_factor_unique, SIMPLIFY = FALSE)

  #Get data_item and data_factor instead of data_item_unique and data_factor_unique ####
  data_item = lapply(X = usename_item,
                     FUN = function(X, data_item_unique){return(data_item_unique[[match(X, table = names(data_item_unique))]])},
                     data_item_unique = data_item_unique)
  data_factor = lapply(X = usename_factor,
                       FUN = function(X, data_factor_unique){return(data_factor_unique[[match(X, table = names(data_factor_unique))]])},
                       data_factor_unique = data_factor_unique)

  #Format based on factor and item ####
  for(i in seq_along(data_data)){
    for(k in snd::grab_mtxKey(data_item[[i]])){
      formated = snd:::formatRI_key2mtx(key = k,
                                        formater = data_item[[i]],
                                        formatee = data_data[[i]],
                                        formaterName = usename_item[[i]],
                                        formateeName = usename_data[[i]])
      data_item[[i]] = formated$formater
      data_data[[i]] = formated$formatee
    }
    for(k in snd::grab_mtxKey(data_factor[[i]])){
      formated = snd:::formatRI_key2mtx(key = k,
                                        formater = data_factor[[i]],
                                        formatee = data_data[[i]],
                                        formaterName = usename_factor[[i]],
                                        formateeName = usename_data[[i]])
      data_factor[[i]] = formated$formater
      data_data[[i]] = formated$formatee
    }
  }

  #Package as SND and return ####
  mapply(FUN = function(data_item, data_data, data_factor){
    return(snd:::classify_set(list(factor = data_factor,
                                   item = data_item,
                                   data = data_data)))},
    data_item = data_item, data_data = data_data, data_factor = data_factor, SIMPLIFY = FALSE) %>%
    setNames(nm = stringr::str_sub(usename_data, start = 7, end = -1L)) %>%
    append(values = list(OS = snd:::classify_os(x = list(DIR = xlsxFile,
                                                         createTime = Sys.time(),
                                                         defaultMod = stringr::str_sub(usename_data, start = 7, end = -1L))))) %>%
    snd:::classify_snd() %>%
    return(invisible(.))
}
