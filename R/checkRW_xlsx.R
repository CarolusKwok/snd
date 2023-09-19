#' @title
#' System tools: Check if the sheets are structured correctly in .xlsx file
#'
#' @description
#' SND requires a organized 2D data structure to represent multivariant data. Additionally, it requires some way that users can easily access and modify the data, even without using R. This function below checks if the .xlsx file is "well-structured", with sheets that describes the datas with factor and items.
#'
#' @param xlsxFile Directory of the .xlsx file
#' @param sheet Name of sheets to be read it, in `character`. Note that only sheets containing data needs to be specified. By default (`NULL`), all available data will be read.
#'
#' @return A message or not
#' @keywords internal
#'
#' @examples checkRW_xlsx(xlsxFile = DIR)
checkRW_xlsx = function(xlsxFile, sheet = NULL){
  #Input check ####
  if(!hasArg(xlsxFile)){snd:::sys_abort_NoArg(x = xlsxFile)}
  snd:::checkFile_xlsx(xlsxFile = xlsxFile)

  ava_factor = snd:::grab_xlsxFactor(xlsxFile = xlsxFile)
  ava_item = snd:::grab_xlsxItem(xlsxFile = xlsxFile)
  ava_data = snd:::grab_xlsxData(xlsxFile = xlsxFile)

  if(is.null(sheet)){
    sheet = ava_data
  } else {
    if(sum(!is.character(sheet))){
      snd:::sys_abort_WrongClass(x = sheet, class = c("character", "NULL"))
    }
    if(sum(duplicated(sheet))){
      sheet = unique(sheet)
      custom =  stringr::str_flatten(string = paste0("{.code ", sheet, "}"), collapse = ", ")
      snd:::sys_warn(message = c("!" = "Duplicated items found in {.arg {arg}}",
                                 "i" = "{.arg {arg}} changed to following",
                                 "i" = custom),
                     x = sheet)
    }
  }

  ##Check if arg `sheet` are present in file as data sheets####
  data_availability = (sheet %in% ava_data)
  if(sum(!data_availability)){
    data_name = sheet[!data_availability]
    snd:::sys_abort_shtUnavailable(x = xlsxFile, unavailable_sheets = data_name)
  }

  ##Check if the data sheets have a corresponding factor sheet ####
  check_factor = function(x, factor){
    x = paste0("#factor", stringr::str_sub(string = x, start = 6L, end = -1L))
    return((x %in% factor) | ("#factor" %in% factor))
  }
  factor_availability = unlist(lapply(X = sheet, FUN = check_factor, factor = ava_factor))
  if(sum(!factor_availability)){
    factor_name = paste0("#factor", stringr::str_sub(string = sheet[!factor_availability], start = 6L, end = -1L))
    snd:::sys_abort_shtUnavailable(x = xlsxFile, unavailable_sheets = factor_name)
  }

  ##Check if the data sheets have a corresponding item sheet####
  check_item = function(x, item){
    x = paste0("#item", stringr::str_sub(string = x, start = 6L, end = -1L))
    return((x %in% item) | ("#item" %in% item))
  }
  item_availability = unlist(lapply(X = sheet, FUN = check_item, item = ava_item))
  if(sum(!item_availability)){
    item_name = paste0("#item", stringr::str_sub(string = sheet[!item_availability], start = 6L, end = -1L))
    snd:::sys_abort_shtUnavailable(x = xlsxFile, unavailable_sheets = item_name)
  }
}
