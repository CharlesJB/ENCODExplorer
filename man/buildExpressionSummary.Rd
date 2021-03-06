% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summaryBuilders.R
\name{buildExpressionSummary}
\alias{buildExpressionSummary}
\title{Calculates average expression levels of the results of a previously
completed ENCODE query.}
\usage{
buildExpressionSummary(
  query_results,
  split_by,
  metric = NULL,
  simplify = FALSE,
  aggregate_function = mean,
  temp_dir = ".",
  force = FALSE
)
}
\arguments{
\item{query_results}{A data.table returned by \code{\link{queryEncode}} or 
\code{\link{queryEncodeGeneric}}.}

\item{split_by}{A vector of column names from query_results that will be used
to split the average expression levels.
If \code{NULL}, all elements of query_results are used in the
same average expression calculation.}

\item{metric}{A regular expression, indicating which column from the ENCODE
data must be extracted. If NULL, ENCODExplorer data 
automatically detects and selects one of the TPM, FPKM or
featurecount columns.}

\item{simplify}{If TRUE, non-discriminatory columns are removed from the metadata,
and if only one sample group is found, it is renamed "All".}

\item{aggregate_function}{A function which takes a vector as input and 
returns a single value summarizing the whole. Used
to summarize expression metrics.}

\item{temp_dir}{The path to a directory where peak files will be 
downloaded.}

\item{force}{A logical indicating whether already present files be 
redownloaded.}
}
\value{
An object of class \linkS4class{ENCODEExpressionSummary}.
}
\description{
This function takes the result of a previous call to 
\code{\link{queryEncode}}, splits the contained expression files by 
conditions (as specified by the \code{split_by} argument), then calculates
average expression levels for each condition.
}
\examples{
query_results = queryEncodeGeneric(biosample_name="neural tube", 
                                   output_type="gene quantifications",
                                   file_type="tsv",
                                   assay="polyA plus RNA-seq",
                                   assembly="^mm10$",
                                   dataset_biosample_summary="(15.5|13.5)",
                                   fixed=FALSE)
                                   
buildExpressionSummary(query_results, split_by="dataset_biosample_summary")

}
