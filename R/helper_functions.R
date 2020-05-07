#' remove_na_cols
#'
#' Remove all columns with nothing but NA values
#' @param df A data frame
#' @export

remove_na_cols <- function(df) {

  require(data.table)

  df <- as.data.table(df)
  df <- df[,which(unlist(lapply(df, function(x)!all(is.na(x))))),with=F]

  df <- as_tibble(df)

  return(df)
}

#' ifnullNA
#'
#' Convert data from NULL to NA using an ifelse approach
#' @param x The data you want to convert

#' @export

ifnullNA <- function(x) {

  z <- ifelse(is.null(x),NA,x)

  return(z)

}

#' nulltoNA
#'
#' Replace NULL with NA
#' @param x Your value
#' @export


nullToNA <- function(x) {
    x[sapply(x, is.null)] <- NA
    return(x)
}

#' start_end_sequencer
#'
#' Get a set of sequences for starting and ending a sequence
#' @param df The dataframe you want to sequence
#' @param increment How much you want your sequence to increment by. Defaults to 100
#' @export

start_end_sequencer <- function(df,increment=100) {

  require(tidyverse)

  start <- seq(from=1,to=nrow(df),by=increment)
  end <- seq(from=increment,to=nrow(df),by=increment)

  if (!nrow(df) %in% end) {

    end <- c(end,nrow(df))
  }

  requests <- tibble(start_request = start,
                     end_request = end)

  return(requests)

}
