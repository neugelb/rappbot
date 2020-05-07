#' appbot_auth
#'
#' Save your API credentials to your environment
#' @param user The API user key
#' @param secret The API secret key
#' @param clear Clear your existing API key so that you can re-enter the user and secret values
#' @export

appbot_auth <- function(user,secret,clear=FALSE) {

  require(keyring)

  if (clear == TRUE) {

    key_delete('appbot_api')
    print('Appbot API key deleted')

  }

  key_set_with_value('appbot_api',username=user,password=secret)
  print('Appbot API key set')

}

appbot_auth_check <- function() {

  require(keyring)
  require(purrr)

  if (is_empty(key_get('appbot_api') == TRUE)) {

    stop('You need to run appbot_auth again to save your API credentials')

  }

  auth_list <- list()

  auth_list$user <- (key_list('appbot_api'))$username
  auth_list$secret <- key_get('appbot_api')

  return(auth_list)

}
