#' @title
#' System tools: A slightly better warning messenger
#'
#' @description
#' Custom `cli` formating can be used to refer different stuff. This function is based on `cli::cli_bullets` for formatting. Note that when writing functions, **do not** use pipe (`%>%`, `|>`) before this function.
#'
#' Currently supported inline-markup themes
#' * .na for NA values,
#' * .mtx for referring to matrices
#' * .col for referring to columns in the matrix
#'
#' @param message Message to be displayed, formatted via `cli::cli_bullets()`. For more information, please read `cli::cli_abort`.
#' @param x An argument name in the current function.
#' @param arg Argument to be traced. When referring to this in argument `message`, use `{.arg {arg}}`.
#' @param ... Additional items to consider in the message. Any items that uses glue (`{}`) must be passed through here.
#'
#' @return A warning message
#' @keywords internal
#'
#' @examples sys_warn(message = "test")
sys_warn = function(message,
                    x,
                    arg = rlang::caller_arg(x),
                    ...){
  #Custom message class####
  cli::cli_div(theme = list(span.na = list("color" = "red", "before" = "#", "after" = "#"),
                            span.mtx = list("color" = "blue",   "text-decoration" = "underline"),
                            span.col = list("color" = "orange", "text-decoration" = "underline")))

  #Pass add to the environment
  list2env(x = list(...), envir = rlang::current_env())

  #Start the message ####
  cli::cli_text(cli::style_bold(cli::col_yellow("Warning"), " occurred"))
  cli::cli_bullets(text = message, .envir = rlang::current_env())
}
