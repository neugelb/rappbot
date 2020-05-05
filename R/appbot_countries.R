#' appbot_countries
#'
#' Gives a list of countries you can access reviews / ratings from
#' @param app_id The ID of the app you want to check
#' @export

appbot_countries <- function(app_id) {

  require(purrr)
  require(jsonlite)
  require(httr)
  require(tidyverse)
  require(keyring)

  aauth <- appbot_auth_check()

  url <- paste('https://api.appbot.co/api/v2/apps',app_id,'countries',sep='/')

  call <- GET(url,
              authenticate(aauth$user,aauth$secret,type='basic'),
              encode = 'json')

  call_content <- content(call,'parsed')

  c <- map(call_content$results,as_tibble)

  c <- do.call('bind_rows',c)

  return(c)

}
