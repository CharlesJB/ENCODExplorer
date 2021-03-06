% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summaryBuilders.R
\name{choose_interactive_value}
\alias{choose_interactive_value}
\title{Interactively select one of many possible metadata values.}
\usage{
choose_interactive_value(values, col_name, allow_all)
}
\arguments{
\item{values}{The vector of values which must be chosen from, extracted from 
the ENCODE metadata.}

\item{col_name}{The name of the column the values were extracted from.}

\item{allow_all}{Whether or not the user can choose 0 to select all values.}
}
\value{
A character vector of chosen values.
}
\description{
Interactively select one of many possible metadata values.
}
\keyword{internal}
