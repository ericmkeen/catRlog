% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match.R
\name{match}
\alias{match}
\title{Match new photo collections to your reference catalog}
\usage{
match(
  collections_to_match = "photos/photos/",
  catalog_photos = "catalog/catalog/",
  catalog_key = "catalog/catalog key.csv",
  filters = c("injury", "nick", "hole")
)
}
\arguments{
\item{collections_to_match}{Path to folder containing subfolders of photo collections that you wish to match against your reference catalog. The default follows the instructions for the \code{catRlog} system setup.}

\item{catalog_photos}{Path to folder with catalog ID photos. The default follows the instructions for the \code{catRlog} system setup.}

\item{catalog_key}{Path to catalog key \code{.csv}. The default follows the instructions for the \code{catRlog} system setup.}

\item{filters}{Character vector of column names within the catalog key that can be used to filter the catalog. Up to three can be used.}
}
\value{
Shiny app. See the \href{https://ericmkeen.github.io/catRlog/}{vignette} for a detailed user guide.
}
\description{
Match new photo collections to your reference catalog
}
