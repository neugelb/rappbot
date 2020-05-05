#' appbot_sources
#'
#' Gives you a list of the sources you've connected to Appbot
#' @export

appbot_sources <- function() {

  require(purrr)
  require(jsonlite)
  require(httr)
  require(tidyverse)
  require(keyring)

  aauth <- appbot_auth_check()

  url <- 'https://api.appbot.co/api/v2/apps'

  call <- GET(url,
              authenticate(aauth$user,aauth$secret,type='basic'),
              encode = 'json')

  call_content <- content(call,'parsed')

  sources <- do.call('bind_rows',map(call_content$results,as_tibble))

  return(sources)

}
