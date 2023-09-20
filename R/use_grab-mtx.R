#' @title
#' System tools: Grab various sheet names from .xlsx file
#'
#' @description
#' SND requires a organized 2D data structure to represent multivariant data. Additionally, it requires some way that users can easily access and modify the data, even without using R. These functions below with a prefix `sys_grab_df` provides the first steps to analysis the organized data structure in a matrix, which is stored as dataframe for ease of use.
#'
#' The functions are as follow:
#' grab_mtxKey: Obtain Keys that will form the matrix
#' grab_mtxFactor: Obtain Factors that will form the matrix
#' grab_mtxItem: Obtain Items that will form the matrix
#'
#' @param dataframe Dataframe itself
#'
#' @return A string of characters
#' @export
#' @rdname grab_mtx
grab_mtxKey = function(dataframe){
  if(rlang::is_missing(dataframe)){snd:::sys_abort_NoArg(dataframe)}
  if(!is.data.frame(dataframe)){snd:::sys_abort_WrongClass(x = dataframe,
                                                           class = c("data.frame", "tbl_df", "tbl"))}

  return(colnames(dataframe) %>%
           .[stringr::str_detect(string = ., pattern = "^@")])
}

#' @export
#' @rdname grab_mtx
grab_mtxFactor = function(dataframe){
  if(rlang::is_missing(dataframe)){snd:::sys_abort_NoArg(dataframe)}
  if(!is.data.frame(dataframe)){snd:::sys_abort_WrongClass(x = dataframe,
                                                           class = c("data.frame", "tbl_df", "tbl"))}

  return(colnames(dataframe) %>%
           .[stringr::str_detect(string = ., pattern = "^#")])
}

#' @export
#' @rdname grab_mtx
grab_mtxItem = function(dataframe){
  if(rlang::is_missing(dataframe)){snd:::sys_abort_NoArg(dataframe)}
  if(!is.data.frame(dataframe)){snd:::sys_abort_WrongClass(x = dataframe,
                                                           class = c("data.frame", "tbl_df", "tbl"))}

  return(colnames(dataframe) %>%
           .[stringr::str_detect(string = ., pattern = "(^@)|(^#)", negate = TRUE)])
}
