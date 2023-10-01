#' Title
#'
#' @param expr
#' @param time
#' @param name
#'
#' @return
#' @export
#'
#' @examples
benchmark = function(expr, time = 5, name = NULL){
  expr = rlang::enexpr(expr)
  index = 1:time
  tests = lapply(X = index,
                 FUN = function(X, expr, time){
                   systemTime = system.time(suppressMessages(eval(expr)))
                   cli::cli_text("Trial {X} completed. {time - X} remaining.")
                   return(systemTime)},
    expr = expr, time = time)

  data = tibble::tibble(user.self = sapply(X = tests, FUN = function(X){return(X["user.self"])}, simplify = TRUE, USE.NAMES = FALSE),
                        sys.self = sapply(X = tests, FUN = function(X){return(X["sys.self"])}, simplify = TRUE, USE.NAMES = FALSE),
                        elapsed = sapply(X = tests, FUN = function(X){return(X["elapsed"])}, simplify = TRUE, USE.NAMES = FALSE),
                        user.child = sapply(X = tests, FUN = function(X){return(X["user.child"])}, simplify = TRUE, USE.NAMES = FALSE),
                        sys.child = sapply(X = tests, FUN = function(X){return(X["sys.child"])}, simplify = TRUE, USE.NAMES = FALSE))
  mean = dplyr::summarise(.data = data,
                          user.self = mean(user.self, na.rm = TRUE),
                          sys.self = mean(sys.self, na.rm = TRUE),
                          elapsed = mean(elapsed, na.rm = TRUE),
                          user.child = mean(user.child, na.rm = TRUE),
                          sys.child = mean(sys.child, na.rm = TRUE))
  sd = dplyr::summarise(.data = data,
                        user.self = sd(user.self, na.rm = TRUE),
                        sys.self = sd(sys.self, na.rm = TRUE),
                        elapsed = sd(elapsed, na.rm = TRUE),
                        user.child = sd(user.child, na.rm = TRUE),
                        sys.child = sd(sys.child, na.rm = TRUE))
  quantiles = dplyr::reframe(.data = data,
                             user.self = quantile(user.self, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                             sys.self = quantile(sys.self, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                             elapsed = quantile(elapsed, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                             user.child = quantile(user.child, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                             sys.child = quantile(sys.child, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))

  if(is.null(name)){
    return(list(data = dplyr::bind_cols(tibble::tibble(trial = index), data),
                stat = dplyr::bind_cols(tibble::tibble(Stat = c("mean", "sd", "P0", "P25", "P50", "P75", "P100")),
                                        dplyr::bind_rows(mean, sd, quantiles))))
  } else {
    return(list(data = dplyr::bind_cols(tibble::tibble(name = name, trial = index), data),
                stat = dplyr::bind_cols(tibble::tibble(name = name,
                                                       stat = c("mean", "sd", "P0", "P25", "P50", "P75", "P100")),
                                        dplyr::bind_rows(mean, sd, quantiles))))
  }
}
