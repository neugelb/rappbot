#' appbot_languages
#'
#' Gives you a list of languages and language codes
#' @export

appbot_languages <- function() {

require(purrr)
require(jsonlite)
require(httr)
require(tidyverse)
require(keyring)

aauth <- appbot_auth_check()

url <- 'https://api.appbot.co/api/v2/detected_languages'

call <- GET(url,
            authenticate(aauth$user,aauth$secret,type='basic'),
            encode = 'json')

call_content <- content(call,'parsed')

langs <- map(call_content$results,as_tibble)

langs <- do.call('bind_rows',langs)

return(langs)

}
