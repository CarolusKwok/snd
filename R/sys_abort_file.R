#' @title
#' System tools: Standard abort message for files
#'
#' @param x The argument holding the file directory.
#'
#' @return A standard abort message
#' @keywords internal
#'
#' @rdname sys_abort_file
sys_abort_fileWrongType = function(x, expected_format){
  snd:::sys_abort(message = c("x" = "Incorrect file format",
                              "!" = "Please input the directory with the correct file format in {.arg {arg}}",
                              "i" = "Expected file format: {.file {expected_format}}"),
                  arg = rlang::caller_arg(x), expected_format = expected_format)
}
