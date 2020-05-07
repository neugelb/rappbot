#' appbot_reviews
#'
#' Get reviews for the app of your choice
#' @param app_id The ID of the app you want to check
#' @param pages How many pages of results you want to provide
#' @param pivot_topics An option to pivot the topics column (which is returned as a list) out to individual columns with a Boolean 1 or 0 to indicate if the review discusses a certain topic
#' @param ... Other arguments include 'page', 'start', 'end', 'keyword', 'country', 'sentiments', 'dlangs', 'version', and 'topic'. For example if you wanted to get reviews from Germany, you would pass 'country=DE' as an argument
#' @export

appbot_reviews <- function(app_id,pages=1,pivot_topics = FALSE,...) {

require(purrr)
require(jsonlite)
require(httr)
require(tidyverse)
require(keyring)

aauth <- appbot_auth_check()

url <- paste('https://api.appbot.co/api/v2/apps',app_id,'reviews',sep='/')

params <- list(...)

if (length(params) == 1) {

url <- paste(url,params,sep='?')

} else if (length(params) > 1) {

  params <- paste(unlist(trimws(params)),collapse = '&')

  url <- paste(url,params,sep='?')

}

if (pages == 1 & (str_detect(url,'start=')==FALSE | str_detect(url,'end=')==FALSE)) {

  call <- GET(url,
              authenticate(aauth$user,aauth$secret,type='basic'),
              encode = 'json')

  call_content <- content(call,'parsed')

  reviews <- reviews_clean(call_content)

} else {

  pages_range <- 1:pages

  slug <- ifelse(str_detect(url,fixed('?'))==FALSE,'?page=','&page=')

  p_url <- paste(url,slug,pages_range[[1]],sep='')

  c1 <- GET(p_url,
            authenticate(aauth$user,aauth$secret,type='basic'),
            encode = 'json')

  cc1 <- content(c1,'parsed')

  total_pages <- cc1$total_pages

  pages <- ifelse(total_pages > pages & (str_detect(url,'start=')==TRUE | str_detect(url,'end=')==TRUE),total_pages,
          ifelse(total_pages < pages,total_pages,pages))

  pages_range <- 1:pages

  reviews <- reviews_clean(cc1)

  for (i in 2:length(pages_range)) {

    slug <- ifelse(str_detect(url,fixed('?'))==FALSE,'?page=','&page=')

    p_url <- paste(url,slug,pages_range[[i]],sep='')

    temp <- GET(p_url,
                authenticate(aauth$user,aauth$secret,type='basic'),
                encode = 'json')

    call_content <- content(temp,'parsed')

    rez <- reviews_clean(call_content)

    reviews <- bind_rows(reviews,rez)

  }

}

reviews <- reviews %>%
  arrange(id,published_at) %>%
  distinct()

if (pivot_topics == TRUE) {

  reviews <- reviews %>%
    reviews_topics_spread()

}

return(reviews)

}

reviews_clean <- function(object) {

  require(purrr)
  require(tidyverse)

  results <- object$results

  results2 <- lapply(results,nullToNA)

  #deal with empty topics lists 

  for (i in 1:length(results2)) {

  if (is_empty(results2[[i]]$topics) == TRUE) {

    results2[[i]]$topics <- NA

  }

}

  results3 <- map(results2,as_tibble)

  results4 <- do.call('bind_rows',results3)

  reviews <- tibble()

  ids <- results4 %>%
    distinct(id) %>%
    pull(id)

  for (i in 1:length(ids)) {

    idfilt <- ids[[i]]

    df2 <- results4 %>%
      filter(id == idfilt) %>%
      unnest(topics)

    t1 <- df2 %>%
      pull(topics)

    df2$topics <- list(t1)

    df3 <- df2 %>%
      distinct()

    reviews <- bind_rows(reviews,df3)

    }

  return(reviews)

}

reviews_topics_spread <- function(df) {

    require(tidyverse)

    df <- df %>%
      unnest(topics) %>%
      mutate(topics = str_to_lower(str_replace_all(topics,' ','_')),
             topics = str_replace_all(topics,'&','and'),
             topics = str_replace_all(topics,'-',''))

    df2 <- tibble()

    vals <- df %>%
      distinct(id) %>%
      pull()

    for (i in 1:length(vals)) {

      val <- vals[[i]]

      z <- df %>%
        filter(id == val) %>%
        distinct()

      z1 <- z %>%
        distinct(topics) %>%
        mutate(values = 1,
               topics = paste('topic',topics,sep='_')) %>%
        pivot_wider(names_from = topics,values_from = values)

      z <- z %>%
        select(-topics) %>%
        distinct() %>%
        bind_cols(z1)

      df2 <- bind_rows(df2,z)

    }

    df2 <- df2 %>%
      mutate_if(is.numeric,funs(ifelse(is.na(.),NA,.)))

    return(df2)

  }
