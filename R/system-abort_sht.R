#' @title
#' System tools: Standard abort message for sheets
#'
#' @param x The argument holding the xlsx.
#'
#' @return A standard abort message
#' @keywords internal
#'
#' @rdname sys_abort_sht
sys_abort_shtUnavailable = function(x, unavailable_sheets){
  if(rlang::is_missing(unavailable_sheets)){snd:::sys_abort_NoArg(unavailable_sheets)}
  unavailable_sheets = stringr::str_flatten(string = paste0("{.code ", unavailable_sheets, "}"),
                                            collapse = ", ")
  snd:::sys_abort(message = c("x" = "Sheet unavailable in {.arg {arg}}",
                              "i" = "Please check if following sheets are available in {.arg {arg}}",
                              "i" = "Sheets not found in {.arg {arg}}:",
                              "i" = unavailable_sheets),
                  arg = rlang::caller_arg(arg = x))
}
