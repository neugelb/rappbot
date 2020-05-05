#' appbot_versions
#'
#' Allows you to get versions data from the Appbot API (iOS only)
#' @param app_id The ID of the app you want to check
#' @param start The start date you want to call in YYYY-MM-DD format
#' @param end The end date you want to call in YYYY-MM-DD format
#' @param country The ID of the country to filter by
#' @param sentiments Sentiments to filter by - could be one or more of 'positive', 'negative', 'neutral', and/or 'mixed'
#' @param dlangs One or more detected languages, using the language ID
#' @param version The version number to filter by
#' @param topic The topic, custom topic or tag ID to filter to
#' @export

appbot_versions <- function(app_id,start=NULL,end=NULL,country=NULL,sentiments=NULL,dlangs=NULL,version=NULL,topic=NULL) {

  require(purrr)
  require(jsonlite)
  require(httr)
  require(tidyverse)
  require(keyring)

  aauth <- appbot_auth_check()

  url_start <- 'https://api.appbot.co/api/v2/apps'

url <- paste(url_start,app_id,'versions',sep='/')

if (is_empty(sentiments) == FALSE) {

  if (length(sentiments)>1) {

    sentiments <- paste(sentiments,collapse='+')

  }
}

if (is_empty(dlangs) == FALSE) {

  if (length(dlangs)>1) {

    dlangs <- paste(dlangs,collapse='+')

  }
}

if (is_empty(start) == FALSE | is_empty(end) == FALSE | is_empty(country) == FALSE | is_empty(sentiments) == FALSE |
    is_empty(dlangs) == FALSE | is_empty(version) == FALSE | is_empty(topic) == FALSE) {

  args_tbl <- tibble(start=ifnullNA(start),
                     end=ifnullNA(end),
                     country=ifnullNA(country),
                     sentiments=ifnullNA(sentiments),
                     dlangs=ifnullNA(dlangs),
                     version=ifnullNA(version),
                     topic=ifnullNA(topic)) %>%
    remove_na_cols()

  if (nrow(args_tbl > 0)) {

    args_vector <- c()

    for (i in 1:ncol(args_tbl)) {

      n <- names(args_tbl)[i]

      v <- args_tbl %>%
        select(i) %>%
        pull()

      nv <- paste(n,v,sep='=')

      args_vector <- c(args_vector,nv)

    }

    url <- paste(url,paste(args_vector,collapse='&'),sep='?')

  }

}

call <- GET(url,
            authenticate(aauth$user,aauth$secret,type='basic'),
            encode = 'json')

call_content <- content(call,'parsed')

if (length(call_content$results)==0) {

  stop('The Appbot API does not provide versions data for Android')

}

versions <- do.call('bind_rows',map(call_content$results,as_tibble))

return(versions)

}
