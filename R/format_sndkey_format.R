#' @export
formatRI_key2mtx.sndkey_format = function(key, formater, formatee, formaterName, formateeName){
  #1. Check if the formats are acceptable ####
  format = snd:::format_sndkeyFormat_rename(col = formater$`@format`)
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
formatWO_key2mtx.sndkey_format = function(key, formater, formatee, formaterName, formateeName){
  #1. Check if the formats are acceptable ####
  format = snd:::format_sndkeyFormat_rename(col = formater$`@format`)
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
  return(snd:::formatWO_key2mtx_sndkey_format(key = key,
                                              formater = formater,
                                              formatee = formatee,
                                              formaterName = formaterName,
                                              formateeName = formateeName))
}
