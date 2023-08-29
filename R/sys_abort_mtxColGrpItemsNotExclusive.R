#' @keywords internal
#' @rdname sys_abort_mtx
sys_abort_mtxColGrpItemsNotExclusive = function(x, name, columns, group_by, failed_groups, exclusive_item){
  if(!hasArg(columns)){snd:::sys_abort_NoArg(columns)}
  if(!hasArg(group_by)){snd:::sys_abort_NoArg(group_by)}
  if(!hasArg(failed_groups)){snd:::sys_abort_NoArg(failed_groups)}
  if(!hasArg(exclusive_item)){snd:::sys_abort_NoArg(exclusive_item)}
  columns = stringr::str_flatten(paste0("{.col ", columns, "}"), collapse = ", ")
  group_by = stringr::str_flatten(paste0("{.col ", group_by, "}"), collapse = ", ")
  failed_groups = stringr::str_flatten(paste0("{.col ", failed_groups, "}"), collapse = ", ")
  exclusive_item = stringr::str_flatten(paste0("{.code ", exclusive_item, "}"), collapse = ", ")

  if(rlang::is_missing(name)){
    snd:::sys_abort(message = c("x" = "Exclusive items in {.arg {arg}} columns, when grouped",
                                "!" = "Please check in the following column:",
                                "!" = columns,
                                "i" = "Grouped by:",
                                "i" = group_by,
                                "i" = "Abnormal groups include:",
                                "i" = failed_groups,
                                "i" = "Exclusive item include:",
                                "i" = exclusive_item),
                    arg = rlang::caller_arg(arg = x))
  } else {
    snd:::sys_abort(message = c("x" = "Exclusive items in {.mtx {name}} columns, when grouped",
                                "!" = "Please check in the following column:",
                                "!" = columns,
                                "i" = "Grouped by:",
                                "i" = group_by,
                                "i" = "Abnormal groups include:",
                                "i" = failed_groups,
                                "i" = "Exclusive item include:",
                                "i" = exclusive_item),
                    arg = rlang::caller_arg(arg = x), name = name)
  }
}
