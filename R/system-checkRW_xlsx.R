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
  if(!hasArg(xlsxFile)){snd:::sys_abort_NoArg(x = xlsxFile)}
  snd:::checkFile_xlsx(xlsxFile = xlsxFile)

  ava_factor = snd:::sys_grab_xlsfactor(xlsxFile = xlsxFile)
  ava_item = snd:::sys_grab_xlsitem(xlsxFile = xlsxFile)
  ava_data = snd:::sys_grab_xlsdata(xlsxFile = xlsxFile)

  if(is.null(sheet)){
    sheet = ava_data
  } else {
    if(sum(!is.character(sheet))){
      snd:::sys_abort(message = c("x" = "{.arg sheet} accepts character/ null only.",
                                      "i" = "Please check {.arg sheet}"))
    }
    if(sum(duplicated(sheet))){
      sheet = unique(sheet)
      custom =  stringr::str_flatten(string = paste0("'", sheet, "'"),
                                     collapse = ", ")
      cli::cli_alert_warning("Duplicated names found in {.arg sheet}.")
      cli::cli_alert_info("{.arg sheet} changed to {.code {custom}}.")
    }
  }

  #Start the check ####
  ##Check if factor has more than 1
  if(length(ava_factor) > 1){
    snd:::sys_abort(message = c("x" = "More than 1 factor sheet in {.arg xlsxFile}",
                                    "i" = "Please check if there are any duplicated '#factor' sheets"))
  }

  ##Check if arg `sheet` are present in file as data sheets####
  data_availability = (sheet %in% ava_data)
  if(sum(!data_availability)){
    data_availability_name = sheet[!data_availability] %>%
      paste0("'", . , "'") %>%
      stringr::str_flatten(collapse = ", ")
    message = c("x" = "sheet unavailable in {.arg xlsxFile}",
                "i" = "Please check if data is available in {.arg xlsxFile}",
                "i" = paste0("Data not found: ", data_availability_name))
    snd:::sys_abort(message = message)
  }

  ##Check if the data sheets have a corresponding item sheet####
  check_item = function(x, item){
    x = paste0("#item",
               stringr::str_sub(string = x, start = 6L, end = -1L))
    return((x %in% item) | ("#item" %in% item))
  }

  item_availability = unlist(lapply(X = sheet, FUN = check_item, item = ava_item))
  if(sum(!item_availability)){
    item_availability_name = sheet[!item_availability] %>%
      paste0("'", . , "'") %>%
      stringr::str_flatten(collapse = ", ")
    message = c("x" = "No corresponding {.code item} in {.arg xlsxFile}",
                "i" = "Please check if item is available in {.arg xlsxFile}",
                "i" = paste0("Data with missing data: ", item_availability_name))
    snd:::sys_abort(message = message)
  }
  return(invisible())
}



