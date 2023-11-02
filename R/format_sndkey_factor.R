#' @keywords internal
format_sndkey_factor_engine = function(key, formater, formatee, formaterName, formateeName){
  ava_factor = formater$`@factor`
  use_factor = snd::grab_mtxFactor(formatee)
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
  test = !(use_factor %in% ava_factor)
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
formatRI_key2mtx.sndkey_factor = function(key, formater, formatee, formaterName, formateeName){
  return(format_sndkey_factor_engine(key = key,
                                     formater = formater,
                                     formatee = formatee,
                                     formaterName = formaterName,
                                     formateeName = formateeName))
}

#' @export
formatWO_key2mtx.sndkey_factor = function(key, formater, formatee, formaterName, formateeName){
  return(format_sndkey_factor_engine(key = key,
                                     formater = formater,
                                     formatee = formatee,
                                     formaterName = formaterName,
                                     formateeName = formateeName))
}
