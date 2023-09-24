#' Obtain unique names from a list
#'
#' @description
#' SND does not tolerate identical names, especially for its datasets as xlsx files can not be saved with identical sheet names!
#' Feed in a list of names in the `name`, and it will pop out a list of names that are completely unique using suffixes.
#'
#' @param name A character vector. Can be names in a list, or column names ...
#'
#' @return A character vector, with all unique names
#' @keywords internal
getUniqueNames = function(name){
  #Check ####
  if(rlang::is_missing(name)){snd:::sys_abort_NoArg(name)}
  if(!is.character(name)){snd:::sys_abort_WrongClass(x = name,
                                                     class = "character")}

  #Find duplicated names ####
  dupName = unique(name[duplicated(name)])
  ## If none, instant return
  if(length(dupName) == 0){return(name)}
  #Start modifying the name ####
  for(d in dupName){
    suffix = sapply(X = 1:length(name),
                    FUN =
                      function(X, inDup){
                        if(!(inDup[X])){
                          return(NA)
                        } else {
                          return(sum(inDup[1:X]))
                        }
                      },
                    inDup = (name == d),
                    simplify = TRUE,
                    USE.NAMES = FALSE)
    name = ifelse(is.na(suffix),
                  name,
                  paste0(name, "(", suffix, ")"))
  }
  return(name)
}
