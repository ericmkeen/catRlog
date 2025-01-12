% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score.R
\name{score}
\alias{score}
\title{Score photos}
\usage{
score(
  collections_to_match = "photos/photos/",
  score_sessions_path = "scores/score sessions/"
)
}
\arguments{
\item{collections_to_match}{Path to folder containing subfolders of photo collections that you wish to score. The default follows the instructions for the \code{catRlog} system setup.}

\item{score_sessions_path}{Path to folder where your scores will be saved. The default follows the instructions for the \code{catRlog} system setup.}
}
\value{
Shiny app. See the \href{https://ericmkeen.github.io/catRlog/}{vignette} for a detailed user guide.
}
\description{
Score photos
}
