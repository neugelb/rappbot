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
