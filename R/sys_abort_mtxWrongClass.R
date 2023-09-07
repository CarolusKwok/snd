#' @keywords internal
#' @rdname sys_abort_mtx
sys_abort_mtxWrongClass = function(x, name){
  class = class(x)
  if(rlang::is_missing(name)){
    snd:::sys_abort(message = c("x" = "Wrong class for {.arg {arg}}",
                                "!" = "You've supplied {.cls {class}}",
                                "i" = "Please use {.cls data.frame}, {.cls tibble} for {.arg {arg}}"),
                    arg = rlang::caller_arg(arg = x), class = class)
  } else {
    snd:::sys_abort(message = c("x" = "Wrong class for {.mtx {name}}",
                                "!" = "You've supplied {.cls {class}}",
                                "i" = "Please use {.cls data.frame}, {.cls tibble} for {.mtx {name}}"),
                    arg = rlang::caller_arg(arg = x), name = name, class = class)
  }
}
