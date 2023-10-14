#' @title
#' System tools: Check if a `.xlsx` file exists
#'
#' @description
#' `snd` can be stored within the computer as a `.xlsx` file. This function checks if the file directory provided is `character(0)`, not existing, or has incorrect extensions (i.e. not `.xlsx`).
#'
#' @param xlsxFile Directory of the xlsxFile
#'
#' @return a standard message (if applicable)
#' @keywords internal
#' @example snd:::checkFile_xlsx(choose.files())
checkFile_xlsx = function(xlsxFile){
  #0. Check input####
  if(rlang::is_missing(xlsxFile)){snd:::sys_abort_NoArg(x = xlsxFile)}
  if(identical(xlsxFile, character(0))){snd:::sys_abort_NoArg(x = xlsxFile)}

  #1. Check file existance ####
  if(!file.exists(xlsxFile)){
    snd:::sys_abort(message = c("x" = "File missing in {.arg xlsxFile}",
                                "!" = "Current input:",
                                "i" = "{.code {arg}}"),
                    arg = rlang::caller_arg(xlsxFile))
  }
  #2. Check if file format is xlsx####
  if(tools::file_ext(xlsxFile) != "xlsx"){
    snd:::sys_abort_fileWrongType(x = xlsxFile,
                                  expected_format = ".xlsx")
  }
}
