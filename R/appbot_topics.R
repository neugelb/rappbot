#' appbot_topics
#'
#' Allows you to call different Appbot API endpoints related to topics
#' @param app_id The ID of the app you want to check
#' @param method The topics method you want to check out - possibilities are 'topics' (for the standard topics) or 'custom_topics'
#' @param start The start date you want to call in YYYY-MM-DD format
#' @param end The end date you want to call in YYYY-MM-DD format
#' @param country The ID of the country to filter by
#' @param sentiments Sentiments to filter by - could be one or more of 'positive', 'negative', 'neutral', and/or 'mixed'
#' @param dlangs One or more detected languages, using the language ID
#' @param version The version number to filter by
#' @export

appbot_topics <- function(app_id,method,start=NULL,end=NULL,country=NULL,sentiments=NULL,dlangs=NULL,version=NULL) {

  require(purrr)
  require(jsonlite)
  require(httr)
  require(tidyverse)
  require(keyring)

  if (!method %in% c('topics','custom_topics')) {

    stop("You've used the wrong method - please check the documentation and run the function again")

  }

  aauth <- appbot_auth_check()

  url_start <- 'https://api.appbot.co/api/v2/apps'

  url <- paste(url_start,app_id,method,sep='/')

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
    is_empty(dlangs) == FALSE | is_empty(version) == FALSE) {

  args_tbl <- tibble(start=ifnullNA(start),
                     end=ifnullNA(end),
                     country=ifnullNA(country),
                     sentiments=ifnullNA(sentiments),
                     dlangs=ifnullNA(dlangs),
                     version=ifnullNA(version)) %>%
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

topics <- do.call('bind_rows',map(map(call_content$results,nullToNA),as_tibble)) %>%
  filter(matches > 0) %>%
  arrange(desc(matches))

return(topics)

}
