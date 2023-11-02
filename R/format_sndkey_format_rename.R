#' @title
#' System tools: Format items within the key `@format`
#'
#' @description
#' To ease data entry within `.xlsx` file, people are allowed to type in abbreviated formats into the `.xlsx` file. However, this is not beneficial for computers! This function reformats everything back to it's full name form.
#'
#'
#' @param col A vector of characters.
#'
#' @return The analysis dataframe, with `full` as the full name of the format
#' @keywords interal
#'
#' @examples
#' snd:::format_sndkeyFormat_rename(col = c("#c", "#i", "i", "c", "lol"))
#' #The last example `lol` should return `match` as `0L` and `fullname` as NA
format_sndkeyFormat_rename = function(col){
  if(rlang::is_missing(col)){snd:::sys_abort_NoArg(col)}

  #0. Prep ####
  sys_format = snd:::sys_format_support(with_abbr = TRUE)
  mtx = data.frame(`format` = col)
  colnames(mtx) = c("@format")

  #1. Start ####
  analysis = data.frame(format = col,
                        prefix = snd::grab_keyPrefix(mtx = mtx, key = "@format"),
                        head = snd:::grab_keyHead(mtx = mtx, key = "@format"),
                        tail = snd::grab_keyTail(mtx = mtx, key = "@format")) %>%
    dplyr::mutate(match_abbr = match(x = head, table = sys_format$abbr, nomatch = 0L),
                  match_alias = match(x = head, table = sys_format$alias, nomatch = 0L),
                  match_full = match(x = head, table = sys_format$full, nomatch = 0L),
                  match = match_abbr + match_alias + match_full,

                  full = ifelse(match == 0, NA, sys_format$full[match]),
                  fullname = ifelse(is.na(prefix), full, paste0(prefix, full)),
                  fullname = ifelse(is.na(tail), fullname, paste0(fullname, "_", tail)))
  return(analysis)
}
