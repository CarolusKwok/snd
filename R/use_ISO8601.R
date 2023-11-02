#' Read In/ Write out Time
#'
#' @description
#' R can not read in time as a ISO8601 string and return as POSIXct. This function reads in ISO8601 strings in `time`, which will also format time in a specific timezone by `tzone`. By default (`""`), the specific timezone will be the local timezone. This function even supports incomplete date-time and negative years.
#' The full format of ISO8601 contains a Date component, a Time component, and a Timezone component, separated by "T" to begin the Time component, i.e. Date**T**TimeTimezone.
#' The date component contains the following format:
#' - YYYY-MM-DD
#' - YYYYMMDD
#' - YYYYMM
#' - YYYY
#'
#' The time component (in 24 hour format) contains the following format:
#' - HH:mm:ss.sss
#' - HHmmss.sss
#' - HH:mm.mmm
#' - HHmm.mmm
#' - HH.hhh
#' - .DDD
#'
#' The timezone component contains the following format:
#' - ±HH:mm
#' - ±HHmm
#' - Z (represents Zulu or UTC time, +00:00)
#'
#' For more information, please visit the wiki page over [here](https://en.wikipedia.org/wiki/ISO_8601).
#'
#'
#' POSIXct does not allow missing date-time components. The missing components will be completed as followed.
#' - Date: Find the median of the missing component, e.g. if only YYYY is filled, the the date will assume it's at 02nd, July of that year
#' - Time: Find the mean of the missing component or fill in by dot, e.g. if only HH is filled, it will assume the time to be half pass HH (i.e. HH:30:00.000); if only the dot is filled (e.g. T.500 or T12.5), it will assume the time to be the amount of the last called time component (i.e. .5 days or .5 hours, which will return 12:00:00.000 and 12:30:00.000 in the previous examples)
#' - Timezone: Find the local time zone.
#'
#' If the string cannot be converted, `NA` will be returned
#'
#' @param time
#' - A vector of character, in ISO8601 (read_ISO8601)
#' - A vector of POSIXct (write_ISO8601)
#' @param tzone A specified timezone in character. Only 1 is allowed. Use `Sys.timezone` to get the local timezone, or `OlsonNames` to get all accepted timezones.
#'
#' @return
#' - A vector of POSIXct (read_ISO8601)
#' - A vector of character in ISO8601 (write_ISO8601)
#' @export
#'
#' @examples
#' read_ISO8601(time = c("2020-01-01T12:00:00Z", "2020-01-01T12.6", "2020-01-01T250000"), tzone = "Hongkong")
#' #which should read "2020-01-01 20:00:00 HKT" "2020-01-01 12:36:00 HKT" NA
#' @rdname ISO8601
read_ISO8601 = function(time, tzone = ""){
  #Check ####
  if(rlang::is_missing(time)){snd:::sys_abort_NoArg(time)}
  if(rlang::is_missing(tzone)){snd:::sys_abort_NoArg(tzone)}
  if(!is.character(time)){snd:::sys_abort_WrongClass(x = time, class = "character")}
  if(!is.character(tzone)){snd:::sys_abort_WrongClass(x = tzone, class = "character")}
  if(length(tzone) != 1){snd:::sys_abort_WrongLength(tzone, 1L)}

  #Accepted Formats ####
  formatDate = "^D?(\\+)?((\\d{8})|(\\d{4}\\-\\d{2}\\-\\d{2}))"
  formatTime = "^T(\\d{2})?(:?\\d{2})?(:?\\d{2})?(\\.\\d{1,3})?(Z|((\\+|\\-)\\d{2}(:?\\d{2})?))?"

  #Get local timezone ####
  time_lc = Sys.time()
  time_UTC = lubridate::with_tz(time_lc, tzone = "UTC")

  tz_lc = (as.numeric(lubridate::date(time_lc) - lubridate::date(time_UTC)) * 24 +
             as.numeric(lubridate::hour(time_lc) - lubridate::hour(time_UTC))) * 60 +
    as.numeric(lubridate::minute(time_lc) - lubridate::minute(time_UTC)) #in minutes
  tz_sg = ifelse(sign(tz_lc), "+", "-")
  tz_lc = abs(tz_lc)
  tz_hr = floor(tz_lc / 60)
  tz_mn = tz_lc - (tz_hr*60)

  tz_str = paste0(tz_sg,
                  stringr::str_pad(tz_hr, width = 2L, side = "left", pad = "0", use_width = TRUE),
                  stringr::str_pad(tz_mn, width = 2L, side = "left", pad = "0", use_width = TRUE))

  #Processing ####
  process = dplyr::mutate(.data = data.frame(str = time),
                          str_date = stringr::str_extract(string = str, pattern = formatDate),
                          str_remain = stringr::str_remove(string = str, pattern = formatDate),
                          str_time = stringr::str_extract(string = str_remain, pattern  = formatTime),

                          #Remove flags, get dots n tz ####
                          str_date = stringr::str_remove_all(string = str_date, pattern = "D"),
                          str_time = stringr::str_remove_all(string = str_time, pattern = "T") %>%
                            ifelse(is.na(.), "12:00:00", .),

                          str_dot = stringr::str_extract(string = str_time, pattern = "\\.\\d{1,3}"),
                          str_tz = stringr::str_extract(string = str_time, pattern = "(Z|(\\+|\\-)\\d{2}:?(\\d{2})?)$"),
                          str_time = stringr::str_remove_all(string = str_time, pattern = "(\\.\\d{1,3})|(Z|(\\+|\\-)\\d{2}:?(\\d{2})?)$"),

                          #Format the date components #####
                          str_date = stringr::str_remove_all(string = str_date, pattern = "\\-"),

                          #Format the tz components ####
                          str_tz = ifelse(is.na(str_tz), tz_str, ifelse(str_tz == "Z", "+00:00", str_tz)),
                          str_tz = stringr::str_pad(string = stringr::str_remove_all(string = str_tz, pattern = ":"),
                                                    width = 5L, side = "right", pad = "0", use_width = TRUE),

                          #Format the time components
                          str_time = stringr::str_remove_all(string = str_time, pattern = ":"),
                          lv_dot = nchar(str_time)/2,
                          lv_dot = ifelse(lv_dot == 3, 1,
                                   ifelse(lv_dot == 2, 60,
                                   ifelse(lv_dot == 1, 3600,
                                                       86400))),
                          str_dot = as.numeric(ifelse(is.na(str_dot), 0, str_dot)),
                          str_time = stringr::str_pad(string = str_time, width = 6L, side = "right", pad = "0", use_width = TRUE),

                          #Bind all the strings ####
                          com_time = paste0(ifelse(is.na(str_date), "", str_date), " T",
                                            ifelse(is.na(str_time), "", str_time)),
                          com_time = as.POSIXct(x = com_time, tz = "UTC", format = "%Y%m%d T%H%M%S"),

                          com_tzsg= ifelse(stringr::str_detect(string = str_tz, pattern = "^\\+"), +1, -1),
                          com_tz  = (as.numeric(stringr::str_sub(string = str_tz, start = 2L, end = 3L)) * 60 +
                                       as.numeric(stringr::str_sub(string = str_tz, start = 4L, end = 5L))) * 60,

                          time = com_time + lubridate::seconds(str_dot * lv_dot) + lubridate::seconds(-com_tzsg * com_tz)
  )
  return(lubridate::with_tz(time = process$time, tzone = tzone))
}

#' @export
#' @rdname ISO8601
write_ISO8601 = function(time){
  #Check ####
  if(rlang::is_missing(time)){snd:::sys_abort_NoArg(time)}
  if(!lubridate::is.POSIXct(time)){snd:::sys_abort_WrongClass(x = time, class = "POSIXct")}
  #Start writng ####
  time = dplyr::mutate(.data = data.frame(time_lc = time,
                                          time_UTC = lubridate::with_tz(time, tzone = "UTC")),

                       #Find timezone value #####
                       tz_lc = (as.numeric(lubridate::date(time_lc) - lubridate::date(time_UTC)) * 24 +
                                  as.numeric(lubridate::hour(time_lc) - lubridate::hour(time_UTC))) * 60 +
                         as.numeric(lubridate::minute(time_lc) - lubridate::minute(time_UTC)), #in minutes
                       tz_sg = ifelse(sign(tz_lc), "+", "-"),
                       tz_lc = abs(tz_lc),
                       tz_hr = floor(tz_lc / 60),
                       tz_mn = tz_lc - (tz_hr*60),
                       tz = paste0(tz_sg,
                                   sprintf("%02d", tz_hr), ":",
                                   sprintf("%02d", tz_mn)),
                       #Find dot value #####
                       time_lc_num = (as.numeric(time_lc) %% 1),
                       time_lc_num = ifelse(stringr::str_sub(string = time_lc_num, start = 1L, end = 2L) == "0.",
                                            stringr::str_sub(string = round(time_lc_num, digits = 3L), start = 3L, end = -1L),
                                            time_lc_num),
                       #Create string ####
                       str = paste0("D",
                                    lubridate::date(time_lc),
                                    "T",
                                    stringr::str_pad(string = lubridate::hour(time_lc), width = 2L, side = "right", pad = "0"), ":",
                                    stringr::str_pad(string = lubridate::minute(time_lc), width = 2L, side = "right", pad = "0"), ":",
                                    stringr::str_pad(string = floor(lubridate::second(time_lc)), width = 2L, side = "right", pad = "0"), ".",
                                    time_lc_num, tz))
  return(time$str)
}
