% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/appbot_versions.R
\name{appbot_versions}
\alias{appbot_versions}
\title{appbot_versions}
\usage{
appbot_versions(
  app_id,
  start = NULL,
  end = NULL,
  country = NULL,
  sentiments = NULL,
  dlangs = NULL,
  version = NULL,
  topic = NULL
)
}
\arguments{
\item{app_id}{The ID of the app you want to check}

\item{start}{The start date you want to call in YYYY-MM-DD format}

\item{end}{The end date you want to call in YYYY-MM-DD format}

\item{country}{The ID of the country to filter by}

\item{sentiments}{Sentiments to filter by - could be one or more of 'positive', 'negative', 'neutral', and/or 'mixed'}

\item{dlangs}{One or more detected languages, using the language ID}

\item{version}{The version number to filter by}

\item{topic}{The topic, custom topic or tag ID to filter to}
}
\description{
Allows you to get versions data from the Appbot API (iOS only)
}
