#' Title
#'
#' @param list
#'
#' @return
#' @export
#'
#' @examples
list_mode = function(list){
  unique_list = unique(list)
  tabulated_list = tabulate(match(list, unique_list))
  return(unique_list[tabulated_list == max(tabulated_list)])
}
