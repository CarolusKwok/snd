#' @title
#' System tools: Check if the sheets are structured correctly in .xlsx file
#'
#' @description
#' SND requires a organized 2D data structure to represent multivariant data. Additionally, it requires some way that users can easily access and modify the data, even without using R. This function below checks if the .xlsx file is "well-structured", with sheets that describes the datas with factor and items.
#'
#' @param xlsxFile Directory of the .xlsx file
#' @param sheet Name of sheets to be read it, in `character`. Note that only sheets containing data needs to be specified (e.g. `"#data_eco"`). By default (`NULL`), all available data will be read.
#'
#' @return A message or not
#' @keywords internal
#'
#' @examples
checkRW_xlsx = function(xlsxFile, sheet = NULL){
  #0. Input check ####
  if(rlang::is_missing(xlsxFile)){snd:::sys_abort_NoArg(x = xlsxFile)}
  if(rlang::is_missing(sheet)){snd:::sys_abort_NoArg(x = sheet)}
  snd:::checkFile_xlsx(xlsxFile = xlsxFile)

  #0. Prep wrok ####
  ava_factor = snd:::grab_xlsxFactor(xlsxFile = xlsxFile)
  ava_item = snd:::grab_xlsxItem(xlsxFile = xlsxFile)
  ava_data = snd:::grab_xlsxData(xlsxFile = xlsxFile)

  #1. Check sheet wrong class, null or duplicate ####
  if(is.null(sheet)){
    sheet = ava_data
  } else {
    if(sum(!is.character(sheet))){
      snd:::sys_abort_WrongClass(x = sheet, class = c("character", "NULL"))
    }
    if(sum(duplicated(sheet))){
      snd:::sys_warn(message = c("!" = "Duplicated items found in {.arg {arg}}",
                                 "i" = "{.arg {arg}} changed to following:",
                                 "i" = snd:::sys_message_sheet(sheet = unique(sheet))),
                     arg = rlang::caller_arg(sheet))
    }
  }

  #2. Check sheet presence####
  data_availability = (sheet %in% ava_data)
  if(sum(!data_availability)){
    snd:::sys_abort(message = c("x" = "Sheets unavailable in {.arg {arg}}",
                                "!" = "Sheets not found in {.arg {arg}}:",
                                "i" = snd:::sys_message_sheet(sheet = sheet[!data_availability]),
                                "i" = "Sheets available in {.arg {arg}}:",
                                "i" = snd:::sys_message_sheet(sheet = ava_data)),
                    arg = rlang::caller_arg(arg = xlsxFile))
  }

  #3. Check corresponding factor sheet####
  factor_availability = sapply(X = sheet,
                               FUN = function(X, ava_factor){
                                 X = paste0("#factor", stringr::str_sub(string = X, start = 6L, end = -1L))
                                 return((X %in% ava_factor) | ("#factor" %in% ava_factor))
                               },
                               ava_factor = ava_factor, simplify = TRUE, USE.NAMES = FALSE)
  if(sum(!factor_availability)){
    failed_sheet = stringr::str_replace(string = sheet[!factor_availability],
                                        pattern = "^#data_", replacement = "#factor_")
    snd:::sys_abort(message = c("x" = "Factor sheets unavailable in {.arg {arg}}",
                                "i" = "Please check if following sheets are available in {.arg {arg}}",
                                "i" = "Sheets not found in {.arg {arg}}:",
                                "i" = snd:::sys_message_sheet(sheet = failed_sheet)),
                    arg = rlang::caller_arg(arg = xlsxFile))
  }

  #4. Check corresponding item sheet####
  item_availability = sapply(X = sheet,
                             FUN = function(X, ava_item){
                               X = paste0("#item", stringr::str_sub(string = X, start = 6L, end = -1L))
                               return((X %in% ava_item) | ("#item" %in% ava_item))
                               },
                             ava_item = ava_item, simplify = TRUE, USE.NAMES = FALSE)
  if(sum(!item_availability)){
    failed_sheet = stringr::str_replace(string = sheet[!item_availability],
                                        pattern = "^#data_", replacement = "#item_")
    snd:::sys_abort(message = c("x" = "Item sheets unavailable in {.arg {arg}}",
                                "i" = "Please check if following sheets are available in {.arg {arg}}",
                                "i" = "Sheets not found in {.arg {arg}}:",
                                "i" = snd:::sys_message_sheet(sheet = failed_sheet)),
                    arg = rlang::caller_arg(arg = xlsxFile))
  }
}
