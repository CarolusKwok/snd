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
                     FUN = snd:::classify_data)

  #Format it accordingly ####
  data_data = mapply(FUN = snd:::formatRI_matrix,
                     mtx = data_data,
                     mtxName = use_data,
                     SIMPLIFY = FALSE, USE.NAMES = FALSE)

  #Forge factors, Factors need @factor @label @format ####
  data_factor = lapply(X = lapply(X = data_data,
                                  FUN = function(X){
                                    factor = snd::grab_mtxFactor(X)
                                    format = sapply(X = factor,
                                                    FUN = function(X, data){
                                                      selected = unlist(unname(dplyr::select(.data = data, {{X}})))
                                                      return(snd:::format_detect(selected))},
                                                    data = X, simplify = TRUE, USE.NAMES = FALSE)
                                    label = sapply(X = factor,
                                                   FUN = function(X, data){
                                                     selected = unlist(unname(dplyr::select(.data = data, {{X}})))
                                                     return(unique(selected))},
                                                   data = X, simplify = FALSE, USE.NAMES = FALSE)
                                    frame = mapply(FUN = function(factor, format, label){
                                      frame = tibble::tibble(`@factor` = factor,
                                                             `@format` = format,
                                                             `@label` = label) %>% as.data.frame
                                      return(frame)},
                                      factor = factor, format = format, label = label, SIMPLIFY = FALSE, USE.NAMES = FALSE)
                                    return(do.call("rbind", frame))}),
                       FUN = snd:::classify_factor)

  #Forge items, Items need @type @format @item ####
  data_item = lapply(X = lapply(X = data_data,
                                FUN = function(X){
                                  item = snd::grab_mtxItem(X)
                                  format = sapply(X = item,
                                                  FUN = function(X, data){
                                                    selected = unlist(unname(dplyr::select(.data = data, {{X}})))
                                                    return(snd:::format_detect(selected))},
                                                  data = X, simplify = TRUE, USE.NAMES = FALSE)
                                  frame = tibble::tibble(`@type` = "data",
                                                         `@item` = item,
                                                         `@format` = format) %>% as.data.frame
                                  return(frame)
                                }),
                     FUN = snd:::classify_item)

  #Normal formatting just like snd::read_xlsx ####
  data_factor = mapply(FUN = snd:::formatRI_matrix,
                       mtx = data_factor, mtxName = use_factor, SIMPLIFY = FALSE)
  data_item = mapply(FUN = snd:::formatRI_matrix,
                     mtx = data_item, mtxName = use_item, SIMPLIFY = FALSE)

  data_data = mapply(FUN = function(data_data, data_item, data_factor, use_data){
    for(k in snd::grab_mtxKey(data_item)){data_data = snd:::formatRI_key2mtx(key = k,
                                                                             formater = data_item,
                                                                             formatee = data_data,
                                                                             formateeName = use_data)}
    for(k in snd::grab_mtxKey(data_factor)){data_data = snd:::formatRI_key2mtx(key = k,
                                                                               formater = data_factor,
                                                                               formatee = data_data,
                                                                               formateeName = use_data)}
    return(data_data)},
    data_data = data_data, data_item = data_item, data_factor = data_factor, use_data = use_data, SIMPLIFY = FALSE)

  #Package as SND and return ####
  mapply(FUN = function(data_item, data_data, data_factor){
    return(snd:::classify_set(list(factor = data_factor,
                                   item = data_item,
                                   data = data_data)))},
    data_item = data_item, data_data = data_data, data_factor = data_factor, SIMPLIFY = FALSE) %>%
    snd:::nameAs(name = stringr::str_sub(use_data, start = 7, end = -1L)) %>%
    append(values = list(OS = snd:::classify_os(x = list(DIR = xlsxFile,
                                                         createTime = Sys.time(),
                                                         defaultMod = stringr::str_sub(use_data, start = 7, end = -1L))))) %>%
    snd:::classify_snd() %>%
    return(invisible(.))
}
