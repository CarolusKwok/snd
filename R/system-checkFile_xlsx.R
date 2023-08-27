#' @title
#' System tools: Check if a file exists
#'
#' @description
#' It checks if a file directory is `character(0)`, not existing, or has incorrect extensions
#'
#' @param xlsxFile Directory of the xlsxFile
#'
#' @return A standard message
#' @keywords internal
#'
#' @examples checkFile_xlsx(xlsxFile)
checkFile_xlsx = function(xlsxFile){
  if(identical(xlsxFile, character(0))){snd:::sys_abort_NoArg(x = xlsxFile)}
  if(!file.exists(xlsxFile)){snd:::sys_abort_FileNotExist(x = xlsxFile)}
  if(tools::file_ext(xlsxFile) != "xlsx"){
    snd:::sys_abort(message = c("x" = "{.arg xlsxFile} is not an {.file .xlsx} file",
                                    "i" = "Please give an {.file .xlsx} file"))
  }
}
