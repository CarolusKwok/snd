#' Title
#'
#' @param xlsxFile
#' @param sheet
#'
#' @return
#' @export
#'
#' @examples
forge_xlsx = function(xlsxFile, sheet){
  #Custom functions ####
  read_workbook = function(X, workbook){
    return(openxlsx::readWorkbook(xlsxFile = workbook, sheet = X))
  }
  classify = function(X, class){
    class(X) = c(class, class(X))
    return(X)
  }
  is_able = function(DATA, CLASS){
    func = paste0("suppressWarnings(as.", CLASS, "(DATA))")
    formated_data = eval(parse(text = func))
    return(sum(is.na(DATA)) == sum(is.na(formated_data)))
  }

  #Check ####
  if(!hasArg(xlsxFile)){snd:::sys_abort_NoArg(xlsxFile)}
  ##Grab all available sheets ####
  ava_data = snd:::grab_xlsxData(xlsxFile = xlsxFile)
  if(!hasArg(sheet)){sheet = ava_data}
  ##Continue ####
  use_data = sheet
  use_factor = "#factor"
  use_item = paste0("#item",
                    stringr::str_sub(use_data, start = 6L, end = -1L))
  #Read in the workbook, get all available styles in the workbook n turn it into string ####
  workbook = openxlsx::loadWorkbook(xlsxFile = xlsxFile)
  for(i in 1:length(openxlsx::getStyles(wb = workbook))){
    openxlsx::replaceStyle(workbook, i, newStyle = openxlsx::createStyle(numFmt = "TEXT"))
  }
  #Start reading in the data of the above use_ list and format all the classes accordingly ####
  data_data = lapply(X = use_data, FUN = read_workbook, workbook = workbook) %>%
    lapply(FUN = classify, class = "snd_data")

  #Format it accordingly ####
  for(i in 1:length(data_data)){data_data[[i]] = snd:::formatRI_matrix(mtx = data_data[[i]], mtxName = use_data[[i]])}

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
                                          return(names(table)[[match(x = TRUE, table = table)]])},
                                        data = X)
                       return(as.data.frame(tibble::tibble(`@item` = items,
                                                           `@format` = formats)))
                     }) %>%
    lapply(FUN = classify, class = "snd_item")
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
                         return(unit_factor)})
  data_factor = dplyr::distinct(do.call("rbind", data_factor)) %>%
    classify(class = "snd_factor")
  #Normal formatting just like snd::read_xlsx ####
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

  #Package as SND ####
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
