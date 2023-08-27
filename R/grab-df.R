#' @title
#' System tools: Grab various sheet names from .xlsx file
#'
#' @description
#' SND requires a organized 2D data structure to represent multivariant data. Additionally, it requires some way that users can easily access and modify the data, even without using R. These functions below with a prefix `sys_grab_df` provides the first steps to analysis the organized data structure in a matrix, which is stored as dataframe for ease of use.
#'
#' The functions are as follow:
#' sys_grab_dfKey: Obtain Keys that will form the matrix
#' sys_grab_dfFactor: Obtain Factors that will form the matrix
#' sys_grab_dfItem: Obtain Items that will form the matrix
#'
#' @param dataframe Dataframe itself
#'
#' @return A string of characters
#' @keywords internal
#' @rdname sys_grab_df
sys_grab_dfKey = function(dataframe){
  colnames = colnames(dataframe)
  Key = stringr::str_sub(string = colnames, start = 1L, end = 1L) %>%
    stringr::str_detect(string = "@")
  colnames = colnames[Key]
  return(colnames)
}

#' @keywords internal
#' @rdname sys_grab_df
sys_grab_dfFactor = function(dataframe){
  colnames = colnames(dataframe)
  Key = stringr::str_sub(string = colnames, start = 1L, end = 1L) %>%
    stringr::str_detect(pattern = "#")
  colnames = colnames[Key]
  return(colnames)
}

#' @keywords internal
#' @rdname sys_grab_df
sys_grab_dfItem = function(dataframe){
  colnames = colnames(dataframe)
  Key = stringr::str_sub(string = colnames, start = 1L, end = 1L) %>%
    stringr::str_detect(pattern = "#|@", negate = TRUE)
  colnames = colnames[Key]
  return(colnames)
}
