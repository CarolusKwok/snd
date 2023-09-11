#' @title
#' System tools: Grab various sheet names from .xlsx file
#'
#' @description
#' SND requires a organized 2D data structure to represent multivariant data. Additionally, it requires some way that users can easily access and modify the data, even without using R. These functions below with a prefix `sys_grab_xls` provides the first steps to analysis the organized data structure in the `.xlsx` file.
#'
#' The functions are as follow:
#' * grab_xlsxFactor: Obtain sheets that will form `factor` matrix
#' * grab_xlsxItem: Obtain sheets that will form `item` matrix
#' * grab_xlsxData: Obtain sheets that will form `data` matrix
#'
#' @param xlsxFile Directory of the excel file
#'
#' @return A string of characters
#' @export
#' @rdname grab_xls
grab_xlsxFactor = function(xlsxFile){
  if(rlang::is_missing(xlsxFile)){snd:::sys_abort_NoArg(xlsxFile)}
  snd:::checkFile_xlsx(xlsxFile = xlsxFile)

  sheet_names = openxlsx::getSheetNames(file = xlsxFile)
  sheet_names = sheet_names[sheet_names == "#factor"]
  return(sheet_names)
}

#' @export
#' @rdname grab_xls
grab_xlsxItem = function(xlsxFile){
  if(rlang::is_missing(xlsxFile)){snd:::sys_abort_NoArg(xlsxFile)}
  snd:::checkFile_xlsx(xlsxFile = xlsxFile)

  sheet_names = openxlsx::getSheetNames(file = xlsxFile)
  sheet_item = stringr::str_sub(sheet_names, start = 1L, end = 6L)
  sheet_names = sheet_names[sheet_item == "#item" | sheet_item == "#item_"]
  return(sheet_names)
}

#' @export
#' @rdname grab_xls
grab_xlsxData = function(xlsxFile){
  if(rlang::is_missing(xlsxFile)){snd:::sys_abort_NoArg(xlsxFile)}
  snd:::checkFile_xlsx(xlsxFile = xlsxFile)
  sheet_names = openxlsx::getSheetNames(file = xlsxFile)
  sheet_data = stringr::str_sub(sheet_names, start = 1L, end = 6L)
  sheet_names = sheet_names[sheet_data == "#data_"]
  return(sheet_names)
}
