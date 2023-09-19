#' Forge in an SND with xlsx
#'
#' @description
#' **WARNING: In general, DO NOT use this function unless you have good reasons.**
#'
#' This function forges an SND, with only the data matrix, following the rules set by the Notion document and in `snd::read_xlsx`. As this function fills in all the necessary components in a SND, this is a good first step to start formatting your xlsx sheets into SND-compatible, along with `snd::write_xlsx`. The data matrix that needs to be SND compatible will need to use the prefix "#data_". If a sheet name in `sheet` is unavailable, the function will abort.
#'
#' @param xlsxFile Directory of the .xlsx file
#' @param sheet Name of sheets to be read it, in `character`. Note that only sheets containing data needs to be specified. By default (`NULL`), all available data will be read.
#'
#' @return A SND object
#' @export
#'
#' @examples forge_xlsx(xlsxFile, sheet = NULL)
forge_xlsx = function(xlsxFile, sheet){
  #Custom functions ####
  read_workbook = function(X, workbook){
    return(openxlsx::readWorkbook(xlsxFile = workbook, sheet = X))
  }
  is_able = function(DATA, CLASS){
    func = paste0("suppressWarnings(as.", CLASS, "(DATA))")
    formated_data = eval(parse(text = func))
    return(sum(is.na(DATA)) == sum(is.na(formated_data)))
  }

  #Check ####
  if(rlang::is_missing(xlsxFile)){snd:::sys_abort_NoArg(xlsxFile)}
  ##Grab all available sheets ####
  ava_data = snd:::grab_xlsxData(xlsxFile = xlsxFile)
  if(!hasArg(sheet)){sheet = ava_data}
  ##Continue ####
  use_data = sheet
  use_factor = paste0("#factor", stringr::str_sub(use_data, start = 6L, end = -1L))
  use_item = paste0("#item", stringr::str_sub(use_data, start = 6L, end = -1L))

  #Read in the workbook, get all available styles in the workbook n turn it into string ####
  workbook = openxlsx::loadWorkbook(xlsxFile = xlsxFile)
  for(i in 1:length(openxlsx::getStyles(wb = workbook))){
    openxlsx::replaceStyle(workbook, i, newStyle = openxlsx::createStyle(numFmt = "TEXT"))
  }
  #Start reading in the data of the above use_ list and format all the classes accordingly ####
  data_data = lapply(X = lapply(X = use_data, FUN = read_workbook, workbook = workbook),
                     FUN = snd:::classify, class = "snd_data")

  #Format it accordingly ####
  data_data = mapply(FUN = snd:::formatRI_matrix,
                     mtx = data_data,
                     mtxName = use_data)

  #Start forging items####
  data_item = lapply(X = data_data,
                     FUN = function(X){
                       items = snd::grab_mtxItem(X)
                       formats = lapply(X = items,
                                        FUN = function(X, data){
                                          selected_data = unlist(data[,match(x = X, table = colnames(data))]) %>%
                                            ifelse(. == "#NA", NA, .)
                                          ##Try format it ####
                                          table = list(logical = !(sum(!(selected_data %in% c("p", NA)))),
                                                       POSIXct = inherits(x = tryCatch(as.POSIXct(selected_data,
                                                                                                  tryFormats = c("%Y%m%d-%H%M%OS", "%Y%m%d-%H%M")),
                                                                                       error = function(e){return("FAIL")}),
                                                                          what = "POSIXct"),
                                                       numeric = is_able(DATA = selected_data, CLASS = "numeric"),
                                                       character = TRUE)
                                          return(unlist(names(table)[[match(x = TRUE, table = table)]]))},
                                        data = X)
                       return(as.data.frame(tibble::tibble(`@item` = unlist(unname(items)),
                                                           `@format` = unlist(unname(formats)),
                                                           `@type` = "data")))
                     }) %>%
    lapply(FUN = snd:::classify, class = "snd_item")
  #Start forging factors####
  data_factor = lapply(X = data_data,
                       FUN = function(X){
                         ##Give it @factor and @label ####
                         factors = snd::grab_mtxFactor(X)
                         unit_factor = X[,match(x = factors, table = colnames(X))] %>%
                           tidyr::pivot_longer(cols = dplyr::all_of(factors),
                                               names_to = "@factor",
                                               values_to = "@label") %>%
                           dplyr::distinct() %>%
                           dplyr::arrange(`@factor`)
                         ##Give it @datatype ####
                         formats = lapply(X = factors,
                                          FUN = function(X, DATA){
                                            used_factor = unlist(dplyr::filter(.data = DATA, `@factor` == X)$`@label`) %>%
                                              ifelse(. == "#NA", NA, .)
                                            table = list(logical = !(sum(!(used_factor %in% c("p", NA)))),
                                                         time = inherits(x = tryCatch(as.POSIXct(used_factor,
                                                                                                 tryFormats = c("%Y%m%d-%H%M%OS", "%Y%m%d-%H%M")),
                                                                                      error = function(e){return("FAIL")}),
                                                                         what = "POSIXct"),
                                                         numeric = is_able(DATA = used_factor, CLASS = "numeric"),
                                                         character = TRUE)
                                            return(names(table)[[match(x = TRUE, table = table)]])},
                                          DATA = unit_factor) %>% unlist
                         unit_factor = dplyr::left_join(x = unit_factor,
                                                        y = tibble::tibble(`@factor` = factors,
                                                                           `@format` = formats),
                                                        by = "@factor")
                         return(as.data.frame(unit_factor))}) %>%
    lapply(FUN = snd:::classify, class = "snd_factor")
  #Normal formatting just like snd::read_xlsx ####
  data_item = mapply(FUN = snd:::formatRI_matrix,
                     mtx = data_item, mtxName = use_item, SIMPLIFY = FALSE)
  data_factor = mapply(FUN = snd:::formatRI_matrix,
                       mtx = data_factor, mtxName = use_factor, SIMPLIFY = FALSE)

  data_data = mapply(FUN = function(data_data, data_item, data_factor, use_data){
    use_key_item = snd::grab_mtxKey(data_item)
    for(k in use_key_item){data_data = snd:::formatRI_key2mtx(key = k,
                                                              formater = data_item,
                                                              formatee = data_data,
                                                              formateeName = use_data)}
    use_key_factor = snd::grab_mtxKey(data_factor)
    for(k in use_key_factor){data_data = snd:::formatRI_key2mtx(key = k,
                                                                formater = data_factor,
                                                                formatee = data_data,
                                                                formateeName = use_data)}
    return(invisible(data_data))},
    data_data = data_data, data_item = data_item, data_factor = data_factor,
    use_data = use_data, SIMPLIFY = FALSE)

  #Package as SND ####
  snd = mapply(FUN = function(data_item, data_data, data_factor){
    snd_set = list(factor = data_factor,
                   item = data_item,
                   data = data_data)
    class(snd_set) = "snd_set"
    return(snd_set)},
    data_item = data_item, data_data = data_data, data_factor = data_factor, SIMPLIFY = FALSE)
  class(snd) = "snd"
  #Give them names ####
  names(snd) = stringr::str_sub(use_data, start = 7, end = -1L)
  return(invisible(snd))
}
