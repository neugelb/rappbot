#' appbot_reviews_import
#'
#' Allows you to call different Appbot API endpoints related to sentiments
#' @param app_id The ID of the app you want to check
#' @param store_id The ID of the app store you are importing to
#' @param author The review author
#' @param rating The review rating
#' @param body The review text body
#' @param subject The review subject
#' @param published_at The review publishing date - if not specified, uses the current date time
#' @param version The app version (iOS only)
#' @param country_code The country code (see appbot_countries for codes)
#' @export

appbot_reviews_import <- function(app_id,store_id,author=NULL,rating=NULL,body=NULL,subject=NULL,version=NULL,country_code=NULL,published_at=NULL) {

  require(purrr)
  require(jsonlite)
  require(httr)
  require(tidyverse)
  require(keyring)

  aauth <- appbot_auth_check()

  url <- paste('https://api.appbot.co/api/v2/apps',app_id,'reviews',sep='/')

  post_body <- list(reviews = list(list(
                    store_id = store_id,
                    author = author,
                    rating = rating,
                    body = body,
                    subject = subject,
                    version = version,
                    country_code = country_code,
                    published_at = published_at)))

  call <- POST(url,
    authenticate(aauth$user,aauth$secret,type='basic'),
    encode = 'json',
    body = post_body)

    if (call$status_code < 300) {

        print('Status code',call$status_code,sep=': ')
        
    } else {

      call_content <- content(call,'parsed')

      print('Status code',call$status_code,sep=': ')

      print(call_content)

    }

}
