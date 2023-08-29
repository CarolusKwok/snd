read_xlsx = function(xlsxFile, sheet){
  #Check ####
  if(!hasArg(xlsxFile)){snd:::sys_abort_NoArg(xlsxFile)}
  ##Grab all available sheets ####
  ava_factor = snd:::grab_xlsxFactor(xlsxFile = xlsxFile)
  ava_item = snd:::grab_xlsxItem(xlsxFile = xlsxFile)
  ava_data = snd:::grab_xlsxData(xlsxFile = xlsxFile)
  if(!hasArg(sheet)){sheet = ava_data}
  ##Continue ####
  snd:::checkRW_xlsx(xlsxFile = xlsxFile, sheet = sheet)

  #Use structure the read in process ####
  use_data = sheet
  use_factor = rep(ava_factor, length(use_data))
  use_item = paste0("#item",
                    stringr::str_sub(use_data, start = 6L, end = -1L)) %>%
    ifelse(. %in% ava_item, .,
           ifelse("#item" %in% ava_item, "#item", "FAIL"))
}
