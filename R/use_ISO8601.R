#' Title
#'
#' @param time
#' @param tzone
#'
#' @return
#' @export
#'
#' @examples
#' @rdname ISO8601
read_ISO8601 = function(time, tzone = ""){
  #Accepted Formats ####
  formatDate = "(^D?(\\+|\\-)?\\d{8})|(^D?(\\+|\\-)?\\d{4}\\-\\d{2}\\-\\d{2})|(^D?(\\+|\\-)?\\d{4}(\\-\\d{2})?)"
  formatTime = "^T(\\d{2})?(:?\\d{2})?(:?\\d{2})?(\\.\\d{1,3})?(Z|((\\+|\\-)\\d{2}(:?\\d{2})?))?"
  origin_date = as.Date(x = "0001-01-01", tz = "UTC")
  #Find local time offsets####
  time_local = as.POSIXct(Sys.time())
  time_UTC = lubridate::with_tz(time_local, tzone = "UTC")
  date_diff = as.numeric(lubridate::as_date(time_local) - lubridate::as_date(time_UTC))
  hour_diff = as.numeric(lubridate::hour(time_local) - lubridate::hour(time_UTC))
  min_diff = as.numeric(lubridate::minute(time_local) - lubridate::minute(time_UTC))
  offset_local = -(date_diff*24*60 + hour_diff*60 + min_diff) #Unit in minutes

  #Start! ####
  process = tibble::tibble(nchar = 0,
                           str = time,
                           str_remain = time) %>%
    dplyr::mutate(date = stringr::str_extract(string = str_remain, pattern = formatDate),
                  str_remain = stringr::str_remove(string = str_remain, pattern = formatDate),
                  time = stringr::str_extract(string = str_remain, pattern = formatTime),
                  str_remain = stringr::str_remove(str_remain, pattern = formatTime),
                  date = ifelse(is.na(date),
                                stringr::str_extract(string = str_remain, pattern = formatDate),
                                date),

                  #Grab dot & tz, remove from time ####
                  dot = stringr::str_extract(time, "\\.\\d{1,3}"),
                  tz = stringr::str_extract(time, "(Z|(\\+|\\-)\\d{2}:?(\\d{2})?)$"),
                  time = stringr::str_remove_all(time, pattern = "(\\.\\d{1,3})|(Z$)|(((\\+|\\-)\\d{2}:?(\\d{2})?)$)"),

                  #If there is still string remaining, there is an error.

                  #Fill in the blanks for date time dot, rephrase Z into 0000 ####
                  date = ifelse(is.na(date), "00010101", date),
                  time = ifelse(is.na(time), "T120000", time),
                  dot = ifelse(is.na(dot), ".000", dot),
                  tz = ifelse(tz == "Z", "+0000", tz),

                  #Grab year sign ####
                  yrSign = ifelse(stringr::str_detect(substr(date, 1, 2), pattern = "\\-"), -1, 0),
                  #Remove leading flags, : , +, -####
                  date = stringr::str_remove_all(date, pattern = "(^D)|\\-|\\+"),
                  time = stringr::str_remove_all(time, pattern = "(^T)|:"),
                  #Format the date. If missing, guess using median ####
                  nchar = nchar(date),
                  date = ifelse(nchar == 4,
                                paste0(date, "0702"),
                                ifelse(nchar == 6,
                                       paste0(date, ifelse(stringr::str_sub(string = Date, start = 5L, end = 6L) == "02", "15", "16")),
                                       date)),
                  date  = sub("(\\d{4})(\\d{2})(\\d{2})", "\\1-\\2-\\3", x = date),

                  #Calculate offset by negative year####
                  offset_yr = lubridate::years(ifelse(yrSign == -1,
                                                      as.numeric(stringr::str_sub(date, 1L, 4L))*2,
                                                      0)),
                  #Guess how the dot will be used ####
                  nchar = nchar(time),
                  dot_use24 = as.integer(nchar < 2),
                  dot_use60 = (nchar < 4) + (nchar < 6),
                  #Add tailing 0s at time and split####
                  time = sub(pattern = "(\\d{2})(\\d{2})(\\d{2})",
                             replacement = "\\1:\\2:\\3",
                             x = stringr::str_pad(string = time, width = 6, side = "right", pad = "0")),

                  #Find the compensation by dot ####
                  offset_dot = lubridate::seconds((as.numeric(dot) * (24^dot_use24) * (60^dot_use60))),

                  #Find the compensation by tz ####
                  tz = stringr::str_pad(string = tz, width = 5L, side = "right", pad = "0"),
                  tzSign = as.integer(ifelse(stringr::str_detect(tz, pattern = "^\\-"), -1, +1)),
                  tzHr = as.integer(stringr::str_sub(string = tz, start = 2L, end = 3L)),
                  tzMn = as.integer(stringr::str_sub(string = tz, start = 4L, end = 5L)),
                  offset_tz = lubridate::minutes(ifelse(is.na(tz),
                                                        offset_local,
                                                        -tzSign * (tzHr * 60 + tzMn))),

                  #Set pass as FALSE if doesnt follow criteria
                  pass = ((!is.na(date) | !is.na(time)) &
                            (is.na(str_remain) | str_remain == "")),

                  #Start Auto formating ####
                  offset_days = lubridate::days(as.numeric(as.Date(date) - offset_yr - origin_date)),
                  offset_time = ISOdatetime(1970, 1, 1,
                                            stringr::str_sub(time, 1L, 2L),
                                            stringr::str_sub(time, 4L, 5L),
                                            stringr::str_sub(time, 7L, 8L), tz = "UTC") -
                    as.POSIXct(x = "1970-01-01 00:00:00", tz = "UTC"),
                  auto = as.POSIXct(x = "0001-01-01 00:00:00", tz = "UTC") + offset_days + offset_time + offset_dot + offset_tz,
                  auto_tzone = lubridate::with_tz(time = auto, tzone = tzone),
                  auto_tzone = ifelse(pass, auto_tzone, NA))

  #Return ####
  return(process$auto_tzone)
}

#' Title
#'
#' @param time
#'
#' @return
#' @export
#'
#' @examples
#' @rdname ISO8601
write_ISO8601 = function(time){
  time = tibble::tibble(time = time,
                        time_UTC = lubridate::with_tz(time, tzone = "UTC")) %>%
    dplyr::mutate(
      #Use the dotted format of str ####
      second = lubridate::second(time) %>% sprintf(fmt = "%.3f", .) %>% stringr::str_pad(width = 6, side = "left", pad = "0"),

      #Find the timezone ####
      offset_day = lubridate::hours((lubridate::as_date(time) - lubridate::as_date(time_UTC))*24),
      offset_hour = lubridate::hours(lubridate::hour(time) - lubridate::hour(time_UTC)),
      offset_min = lubridate::minutes(lubridate::minute(time) - lubridate::minute(time_UTC)),
      offset_total = as.numeric((offset_day + offset_hour + offset_min)),
      offset_remain = offset_total,
      #TZ!
      TZ_sign = sign(offset_total),
      offset_remain = abs(offset_total),
      TZ_hr   = floor(offset_remain/60/60),
      offset_remain = offset_remain - (TZ_hr*60*60),
      TZ_min  = floor(offset_remain/60),
      TZ = paste0(ifelse(TZ_sign == -1, "-", "+"),
                  sprintf("%02d", TZ_hr), ":",
                  sprintf("%02d", TZ_min)),

      str = ifelse((is.na(time) | (!inherits(x = time, what = "POSIXct"))),
                   NA_character_,
                   paste0("D",
                          sprintf("%04d", lubridate::year(time)), "-",
                          sprintf("%02d", lubridate::month(time)), "-",
                          sprintf("%02d", lubridate::day(time)),
                          "T",
                          sprintf("%02d", lubridate::hour(time)), ":",
                          sprintf("%02d", lubridate::minute(time)), ":",
                          second, TZ)),
    )
  return(time$str)
}
