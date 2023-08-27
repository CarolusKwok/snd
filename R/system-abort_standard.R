#' @title
#' System tools: Standard abort message
#'
#' @param x The missing argument
#' @param class Class
#'
#' @return A standard abort message
#' @keywords internal
#'
#' @examples sys_abort_NoArg(x = xlsxFile)
#' @rdname sys_abort
sys_abort_NoArg = function(x){
  snd:::sys_abort(message = c("x" = "Missing {.arg {arg}}",
                                  "i" = "Please enter {.arg {arg}}"),
                      arg = rlang::caller_arg(arg = x))
}

#' @keywords internal
#' @rdname sys_abort
sys_abort_FileNotExist = function(x){
  snd:::sys_abort(message = c("x" = "File missing in {.arg {arg}}",
                                  "i" = "Please give file path in {.arg {arg}}"),
                      arg = rlang::caller_arg(arg = x))
}


#' @keywords internal
#' @rdname sys_abort
sys_abort_WrongClass = function(x, class){
  class = stringr::str_flatten(string = class, collapse = ", ")
  custom_message = c("x" = "Wrong class in {.arg {arg}}",
                     "i" = paste0("Please use {.cls ", class, "} in {.arg {arg}}"))
  snd:::sys_abort(message = custom_message,
                      arg = rlang::caller_arg(arg = x))
}

