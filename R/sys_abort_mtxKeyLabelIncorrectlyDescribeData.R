#' @keywords internal
#' @rdname sys_abort_mtx
sys_abort_mtxKeyLabelIncorrectlyDescribeData = function(x, name, failed_columns){
  if(rlang::is_missing(x)){snd:::sys_abort_NoArg(x)}
  if(rlang::is_missing(failed_columns)){snd:::sys_abort_NoArg(failed_columns)}
  failed_columns = stringr::str_flatten(string = paste0("{.col ", failed_columns, "}"), collapse = ", ")

  if(rlang::is_missing(name)){
    snd:::sys_abort(message = c("x" = "Describing columns incorrectly in {.arg {arg}} using {.col @label}",
                                "!" = "Incorrectly described columns:",
                                "!" = failed_columns,
                                "i" = "Please check {.arg {arg}}"),
                    arg = rlang::caller_arg(arg = x))
  } else {
    snd:::sys_abort(message = c("x" = "Describing columns incorrectly in {.mtx {name}} using {.col @label}",
                                "!" = "Incorrectly described columns:",
                                "!" = failed_columns,
                                "i" = "Please check {.mtx {name}}"),
                    arg = rlang::caller_arg(arg = x), name = name)
  }
}
