#' appbot_ratings_ios
#'
#' Get ratings for an iOS app
#' @param app_id The ID of the app you want to check
#' @param method Either 'historical' or 'live'
#' @param output Either 'current_version' or 'all_time'
#' @param country The two letter country code that is used in iOS, like 'DE' or 'IT'
#' @param start The start date to search in
#' @param end The end date to search in
#' @export

appbot_ratings_ios <- function(app_id,method,output,country,start=NULL,end=NULL) {

    require(purrr)
    require(jsonlite)
    require(httr)
    require(tidyverse)
    require(keyring)

    aauth <- appbot_auth_check()

    url <- paste('https://api.appbot.co/api/v2/apps',app_id,'ratings',sep='/')

    if (method == 'historical') {

        url <- paste(url,'historical',sep='/')

        url <- paste(url,'?','end=',end,'&start=',start,'&country=',country,sep='')

    } else if (method == 'live') {

        url <- paste(url,'?','country=',country,sep='')

    } else {

        stop('Method needs to be "live" or "historical"')

    }

    call <- GET(url,
                authenticate(aauth$user,aauth$secret,type='basic'),
                encode = 'json')

    call_content <- content(call,'parsed')

    if (output == 'current_version') {

        r <- call_content$current_version

        if (method == 'historical') {

            ratings <- do.call('bind_rows',map(r,ratings_clean))

        } else {

            ratings <- ratings_clean(r)

        }

    } else {

        r <- call_content$all_time

        if (method == 'historical') {

            ratings <- do.call('bind_rows',map(r,ratings_clean))

        } else {

            ratings <- ratings_clean(r)

        }

    }

    return(ratings)

}

#' appbot_ratings_android
#'
#' Get ratings for an Android app
#' @param app_id The ID of the app you want to check
#' @param method Either 'historical' or 'live'
#' @param start The start date to search in
#' @param end The end date to search in
#' @export

appbot_ratings_android <- function(app_id,method,start=NULL,end=NULL) {

  require(purrr)
  require(jsonlite)
  require(httr)
  require(tidyverse)

  aauth <- appbot_auth_check()

  url <- paste('https://api.appbot.co/api/v2/apps',app_id,'ratings',sep='/')

  if (method == 'historical') {

    url <- paste(url,'historical',sep='/')

    url <- paste(url,'?','end=',end,'&start=',start,sep='')

  } else if (!method %in% c('live','historical')) {

    stop('Method needs to be "live" or "historical"')

  }

  call <- GET(url,
              authenticate(aauth$user,aauth$secret,type='basic'),
              encode = 'json')

  call_content <- content(call,'parsed')

  at <- call_content$all_time

  if (method == 'live') {

    ratings <- ratings_clean(at)

  } else {

    ratings <- do.call('bind_rows',map(at,ratings_clean))

  }

return(ratings)

}

ratings_clean <- function(obj) {

  require(tidyverse)
  require(lubridate)

  obj <- nullToNA(obj)
  obj$breakdown <- nullToNA(obj$breakdown)

  r <- as_tibble(obj) %>%
    unnest(breakdown) %>%
    mutate(created_at = as.POSIXct(created_at),
         date = ymd(created_at),
         stars = paste('stars',5:1,sep='_')) %>%
    pivot_wider(names_from = stars,values_from = breakdown)

  return(r)

}
