#' Find the statistical mode of a list.
#'
#' A list may contain many different classes of objects. This function returns the object that appears in the list the most, regardless of object class.
#'
#' @param list the list of objects
#'
#' @return A list of objects, that it's item(s) are the statistical mode of the given list
#' @export
#'
#' @examples list_mode(list)
list_mode = function(list){
  if(rlang::is_missing(list)){snd:::sys_abort_NoArg(list)}

  unique_list = unique(list)
  tabulated_list = tabulate(match(list, unique_list))
  return(unique_list[tabulated_list == max(tabulated_list)])
}
