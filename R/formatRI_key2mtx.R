#' System tools: Checking and Formatting other matrixs using keys during read-in.
#'
#' @description
#' In SND, a `key` is a column within the matrix that can be used to check and modify itself, it's matrix and other matrices. This function uses a key within `formater`, to checks and modifies another matrix `formatee`, using a key. To modify it's own matrix, check the function `snd:::formatRI_key`. To enhance security, only 1 key will be accepted.
#'
#' @param key The key in `character`. Only 1 key will be accepted.
#' @param formater The matrix in `data.frame` to format the formatee. The matrix must contain the key.
#' @param formatee The matrix in `data.frame` to be formatted using formater's keys.
#' @param formaterName
#' @param formateeName The name of the formatee in `character`.
#'
#' @return The formatee matrix, checked (and formatted) by formater matrix.
#' @export
formatRI_key2mtx = function(key, formater, formatee, formaterName, formateeName){
  #Check ####
  if(rlang::is_missing(key)){snd:::sys_abort_NoArg(key)}
  if(rlang::is_missing(formater)){snd:::sys_abort_NoArg(formater)}
  if(rlang::is_missing(formatee)){snd:::sys_abort_NoArg(formatee)}
  if(rlang::is_missing(formaterName)){snd:::sys_abort_NoArg(formaterName)}
  if(rlang::is_missing(formateeName)){snd:::sys_abort_NoArg(formateeName)}
  if(length(key) != 1){snd:::sys_abort_WrongLength(x = key, length = 1L)}
  valid_key = key %in% snd::grab_mtxKey(formater)
  if(!valid_key){snd:::sys_abort_mtxMissingSelectedKey(x = formater, keys_missing = key, name = formaterName)}

  #Call other functions to format using the keys ####
  key = snd:::classify_key(stringr::str_remove(string = key, pattern = "^@"))
  UseMethod(generic = "formatRI_key2mtx", object = key)
}

#' @export
formatRI_key2mtx.sndkey_factor = function(key, formater, formatee, formaterName, formateeName){
  ava_factor = formater$`@factor`
  #1. Check if @factor is all filled ####
  test = sum(is.na(ava_factor) | ava_factor == "#NA")
  if(test){
    snd:::sys_abort(message = c("x" = "{.col @factor} not filled in {.mtx {formaterName}}",
                                "i" = "{.col @factor} must be all filled to function properly",
                                "i" = "{.code #NA} is not accepted"),
                    formaterName = formaterName)
  }

  #2. Check if @factor contains items ####
  test = !stringr::str_detect(string = ava_factor, pattern = "^#")
  if(sum(test)){
    snd:::sys_abort(message = c("x" = "{.mtx {formaterName}} contain items in {.col @factor}",
                                "!" = "Items include:",
                                "i" = snd:::sys_message_code(code = unique(ava_factor[test]))),
                    formaterName = formaterName)
  }

  #3. Checks if the formatee described ####
  use_factor = snd::grab_mtxFactor(formatee)
  test = !(use_factor %in% unique(formater$`@factor`))
  if(sum(test)){
    snd:::sys_abort(message = c("x" = "Missing factors in {.mtx {formaterName}} {.col @factor}",
                                "i" = "Factors missing:",
                                "i" = snd:::sys_message_columns(columns = unique(use_factor[test]))),
                    formaterName = formaterName)
  }
  #Return ####
  return(list(formater = formater,
              formatee = formatee))
}

#' @export
formatRI_key2mtx.sndkey_item = function(key, formater, formatee, formaterName, formateeName){
  ava_item = formater$`@item`
  #1. Check if @item is all filled ####
  test = sum(is.na(ava_item) | ava_item == "#NA")
  if(test){
    snd:::sys_abort(message = c("x" = "{.col @item} not filled in {.mtx {formaterName}}",
                                "i" = "{.col @item} must be all filled to function properly",
                                "i" = "{.code #NA} is not accepted"),
                    formaterName = formaterName)
  }
  #2. Check if @item contains factors ####
  test = stringr::str_detect(string = ava_item, pattern = "^#")
  if(sum(test)){
    snd:::sys_abort(message = c("x" = "{.mtx {formaterName}} contain factors in {.col @item}",
                                "!" = "Factors include:",
                                "i" = snd:::sys_message_code(code = unique(ava_item[test]))),
                    formaterName = formaterName)
  }
  #3. Checks if the formatee described ####
  use_item = snd::grab_mtxItem(formatee)
  test = !(use_item %in% unique(formater$`@item`))
  if(sum(test)){
    snd:::sys_abort(message = c("x" = "Missing items in {.mtx {formaterName}} {.col @item}",
                                "i" = "Items missing:",
                                "i" = snd:::sys_message_columns(columns = unique(use_item[test]))),
                    formaterName = formaterName)
  }
  #Return ####
  return(list(formater = formater,
              formatee = formatee))
}

#' @export
formatRI_key2mtx.sndkey_type = function(key, formater, formatee, formaterName, formateeName){
  #Doesn't really interact with other matrices
  #1. Check if @type if of the following classes ####
  return(list(formater = snd:::formatRI_key.sndkey_type(key = key,
                                                        mtx = formater,
                                                        mtxName = formaterName),
              formatee = formatee))
}

#' @export
formatRI_key2mtx.sndkey_format = function(key, formater, formatee, formaterName, formateeName){
  #1. Check if the formats are acceptable ####
  sys_format = snd:::sys_format_support(with_abbr = TRUE)
  format = data.frame(format = formater$`@format`,
                      prefix = snd::grab_keyPrefix(mtx = formater, key = "@format"),
                      head = snd:::grab_keyHead(mtx = formater, key = "@format"),
                      tail = snd::grab_keyTail(mtx = formater, key = "@format")) %>%
    dplyr::mutate(match_abbr = match(x = head, table = sys_format$abbr, nomatch = 0L),
                  match_alias = match(x = head, table = sys_format$alias, nomatch = 0L),
                  match_full = match(x = head, table = sys_format$full, nomatch = 0L),
                  match = match_abbr + match_alias + match_full,

                  full = ifelse(match == 0, NA, sys_format$full[match]),
                  fullname = ifelse(is.na(prefix), full, paste0(prefix, full)),
                  fullname = ifelse(is.na(tail), fullname, paste0(fullname, "_", tail)))

  if(sum(format$match == 0)){
    snd:::sys_abort(message = c("x" = "Unsupported format specified in {.mtx {formaterName}} {.col @format}",
                                "!" = "Unsupported format:",
                                "!" = snd:::sys_message_code(code = unique(dplyr::filter(.data = format, match == 0)$format)),
                                "i" = "Please specify supported format in {.col @format}",
                                "i" = "Check supported format using the following command:",
                                "i" = "{.code snd:::sys_format_support(with_abbr = TRUE)}",
                                "i" = "Supply using any strings in {.code full}, {.code alias}, {.code abbr}",
                                "i" = "A {.code #} prefix specifies it to be a {.cls factor}"),
                    formaterName = formaterName)
  }

  #2. Apply the formatted format into formater ####
  formater = dplyr::mutate(.data = formater,
                           `@format` = format$fullname)

  #3. Use Methods ####
  return(snd:::formatRI_key2mtx_sndkey_format(key = key,
                                              formater = formater,
                                              formatee = formatee,
                                              formaterName = formaterName,
                                              formateeName = formateeName))
}

#' @export
formatRI_key2mtx.sndkey_label = function(key, formater, formatee, formaterName, formateeName){
  #Use methods####
  return(snd:::formatRI_key2mtx_sndkey_label(key = key,
                                             formater = formater,
                                             formatee = formatee,
                                             formaterName = formaterName,
                                             formateeName = formateeName))
}
