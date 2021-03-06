% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summaryBuilders.R
\name{queryTranscriptExpression}
\alias{queryTranscriptExpression}
\title{Queries and returns average transcript expression level for a given 
biosample_name.}
\usage{
queryTranscriptExpression(
  biosample_name,
  assay = NULL,
  assembly = NULL,
  simplify = TRUE,
  use_interactive = FALSE
)
}
\arguments{
\item{biosample_name}{The cell-line/tissue for which average expression 
levels should be queried.}

\item{assay}{The assay type to summarize. If \code{NULL}, the most generic
assay type is automatically selected.}

\item{assembly}{The target genomic assembly. If \code{NULL}, the most recent 
available assembly is selected.}

\item{simplify}{If TRUE, non-discriminatory columns are removed from the metadata,
and if only one sample group is found, it is renamed "All".}

\item{use_interactive}{If TRUE, the user will be prompted to select prefered
metadata values when multiple possibilities are
available.}
}
\value{
An object of class \linkS4class{ENCODEExpressionSummary}.
}
\description{
ENCODE files are automatically split by biosample_description (which will
separate samples from different cell fractions or sequencing methods) and by
the treatment columns.
}
\examples{
  queryTranscriptExpression("bone marrow")
}
\seealso{
\code{\link{buildExpressionSummary}}, 
         \code{\link{queryGeneExpression}}
}
