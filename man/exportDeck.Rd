% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decks.R
\name{exportDeck}
\alias{exportDeck}
\title{Export a Crunch Deck}
\usage{
exportDeck(deck, file, format = c("xlsx", "pptx", "json"), ...)
}
\arguments{
\item{deck}{A CrunchDeck}

\item{file}{The file path to save the exported deck}

\item{format}{Either \code{"xlsx"}, \code{"pptx"}, or \code{"json"}}

\item{...}{Further options to be passed on to the API}
}
\value{
the filename (\code{file}, if specified, or the the autogenerated file
name).
}
\description{
Crunch decks can be exported as excel or json files.
}
