% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download.R
\name{downloadEncode}
\alias{downloadEncode}
\title{downloadEncode is used to download a serie of files or datasets 
using their accession.}
\usage{
downloadEncode(
  file_acc = NULL,
  df = get_encode_df(),
  format = "all",
  dir = ".",
  force = TRUE
)
}
\arguments{
\item{file_acc}{A \code{character} of ENCODE file or 
experiment accessions. Can also be a data.table coming from any ENCODExplorer
search function.}

\item{df}{The reference \code{data.table} used to find the download. Files 
that are not available will be searched directly through the current
ENCODE database.}

\item{format}{The specific file format to download.
Default : all}

\item{dir}{The directory to locate the downloaded files}

\item{force}{\code{boolean} to allow downloading a file even if it already exists
in the directory. 
Default : TRUE}
}
\value{
A \code{character} with the downloaded files
}
\description{
downloadEncode is used to download a serie of files or datasets 
using their accession.
}
\examples{
 fuzzy_result <- fuzzySearch("ENCSR396EAG", get_encode_df_demo(), filterVector = "accession")
 \dontrun{downloadEncode(fuzzy_result, format="tsv")}

}
