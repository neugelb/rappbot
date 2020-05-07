#' appbot_reviews_import
#'
#' Allows you to call different Appbot API endpoints related to sentiments
#' @param app_id The ID of the app you want to check
#' @param reviews_df A data frame of reviews that you want to import - must contain the following columns:
#' \itemize{
#'   \item store_id The app store ID - you can get this from \code{\link{appbot_reviews}}
#'   \item author The review author
#'   \item rating The review rating
#'   \item body The review text body
#'   \item subject The review subject
#'   \item published_at The review publishing date
#'   \item version The app version (iOS only)
#'   \item country_code The country code (see appbot_countries for codes)
#' }
#' @export

appbot_reviews_import <- function(app_id,reviews_df) {

  require(purrr)
  require(jsonlite)
  require(httr)
  require(tidyverse)
  require(keyring)
  require(rlist)

  aauth <- appbot_auth_check()

#check for expected columns

  expected_columns <- c("store_id", "author", "rating", "body", "subject", "country_code",
"version", "published_at")

expected_check <- expected_columns %in% names(reviews_df)

if (all(expected_check) == FALSE) {

  stop('Not all of the expected columns are present - check the documentation and update your data frame appropriately')

}

if (nrow(reviews_df) > 100) {

  stop('You can only deliver 100 reviews per API call - please split your DF into batches')

}

  url <- paste('https://api.appbot.co/api/v2/apps',app_id,'reviews',sep='/')

#convert the reviews to a list

  review_list <- list()

  for (i in 1:nrow(reviews_df)) {

    x <- reviews_df %>%
      slice(i)

    xlist <- list(
      store_id = x$store_id,
      author = x$author,
      rating = x$rating,
      body = x$body,
      subject = x$subject,
      version = x$version,
      country_code = x$country_code,
      published_at = x$published_at)

    review_list <- list.append(review_list,xlist)

  }

  #add the reviews to the post_body

  post_body <- list(reviews = review_list)

  #make the API call

  call <- POST(url,
    authenticate(aauth$user,aauth$secret,type='basic'),
    encode = 'json',
    body = post_body)

    #report the results

    if (call$status_code < 300) {

        print(paste('Status code',call$status_code,sep=': '))

    } else {

      call_content <- content(call,'parsed')

      print(paste('Status code',call$status_code,sep=': '))

      print(call_content)

    }

}
