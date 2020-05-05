#' appbot_auth
#'
#' Save your API credentials to your environment
#' @param user The API user key
#' @param secret The API secret key
#' @export

appbot_auth <- function(user,secret) {

    require(keyring)

    key_set_with_value('appbot_api',username=user,password=secret)

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
