#' ENCODESummary objects: summaries of multiple ENCODE files.
#'
#' ENCODESummary objects is the base class of 
#' \link{ENCODEBindingConsensus-class} and 
#' \link{ENCODEExpressionSummary-class} objects. It provides methods to query
#' which files were used to build the summary, the names of the grouped 
#' elements as well as their metadata.
#'
#' @slot files The path of the files used in this summary.
#' @slot file_metadata A list of data-frames representing the ENCODE metadata
#'                     of the files used to build the per-condition consensus.
#' @slot metadata A data-frame with the metadata of each element in the summary.
#'
#' @param x The \linkS4class{ENCODESummary} object.
#' @param object The \linkS4class{ENCODESummary} object.
#' @param value The new names for the elements of the 
#'              \linkS4class{ENCODESummary} object.
#'
#' @section Methods:
#'
#'   ENCODESummary object can be accessed through a variety of methods:
#'
#'   \describe{
#'      \item{\code{names}}{Returns the names of the elements.}
#'      \item{\code{names<-}}{Sets the names of the elements.}
#'      \item{\code{length}}{Returns the number of elements.}
#'      \item{\code{files}}{Returns a character vector of the ENCODE files used 
#'                          to build this object.}
#'      \item{\code{file_metadata}}{Returns a list of per-condition metadata of 
#'                                  the ENCODE files used to build the object.}
#'      \item{\code{metadata}}{Returns a data-frame of the common per-condition 
#'                             metadata of the ENCODE files used to build the 
#'                             object.}
#'      \item{\code{show}}{Print a summary of the object.}
#'   }
#'
#'
#' @return
#'    For \code{names}, \code{names<-}, a copy of the object. For \code{length},
#'    the number of elements. For \code{files}, a character vector. For 
#'    \code{file_metadata}, a list of data-frames with each file's metadata. For
#'    \code{metadata}, a data-frame with the discriminating metadata of each 
#'    sample group.
#'
#' @examples
#'   res = queryConsensusPeaks("22Rv1", "GRCh38", "CTCF")
#'   names(res)
#'   files(res)
#'   metadata(res)
#'   print(res)
#'
#' @name ENCODESummary-class
#' @rdname ENCODESummary-class
#' @export
setClass("ENCODESummary",
         slots=list(files="character",
                    file_metadata="list",
                    metadata="data.frame"))

#' @export
#' @rdname ENCODESummary-class
setMethod("names",
          c(x="ENCODESummary"),
          function(x) {
            names(x@file_metadata)
          })

#' @export
#' @rdname ENCODESummary-class
setMethod("names<-",
          c(x="ENCODESummary", value="character"),
          function(x, value) {
            rownames(x@metadata) <- value
            names(x@file_metadata) <- value
            x
          })

#' @export
#' @rdname ENCODESummary-class    
setMethod("length",
          c(x="ENCODESummary"),
          function(x) {
            length(x@file_metadata)
          }) 


#' @docType methods
#' @rdname ENCODESummary-class
#' @export
setGeneric("metadata", function(x) standardGeneric("metadata"))

#' @rdname ENCODESummary-class
#' @aliases metadata,ENCODESummary,ENCODESummary-method
setMethod("metadata",
          c(x="ENCODESummary"),
          function(x) {
            x@metadata
          })

#' @docType methods
#' @rdname ENCODESummary-class
#' @export
setGeneric("file_metadata", function(x) standardGeneric("file_metadata"))

#' @rdname ENCODESummary-class
#' @aliases file_metadata,ENCODESummary,ENCODESummary-method
setMethod("file_metadata",
          c(x="ENCODESummary"),
          function(x) {
            x@file_metadata
          })

#' @docType methods
#' @rdname ENCODESummary-class
#' @export
setGeneric("files", function(x) standardGeneric("files"))

#' @rdname ENCODESummary-class
#' @aliases files,ENCODESummary,ENCODESummary-method
setMethod("files",
          c(x="ENCODESummary"),
          function(x) {
            x@files
          })

#' @export
#' @rdname ENCODESummary-class
setMethod("show", "ENCODESummary",
          function(object) {
            cat("Summarizing", length(files(object)), "ENCODE files into",
                length(object), "categories.\n\n")
            cat("Metadata:\n")
            print(metadata(object))
          })

#' ENCODEBindingConsensus: consensus peaks derived from ENCODE files.
#'
#' ENCODEBindingConsensus objects represent the intersection of called peaks
#' across multiple replicates, split by arbitrary metadata columns. They can be
#' constructed using the \link{queryConsensusPeaks} and 
#' \link{buildConsensusPeaks} functions.
#'
#' @slot peaks The per-condition original peaks used to build the consensus.
#' @slot consensus The per-condition consensus peaks.
#' @slot consensus_threshold The proportion of replicates which must bear
#'                           a specific peak for it to be added to the set of
#'                           consensus peaks.
#'
#' @param x The \linkS4class{ENCODESummary} object.
#' @param object The \linkS4class{ENCODESummary} object.
#' @param value The new names for the elements of the 
#'              \linkS4class{ENCODESummary} object.
#'
#' @section Methods:
#'
#'   ENCODEBindingConsensus object can be accessed through the methods from the 
#'   ENCODESummary class, as well as ENCODEBindingConsensus-specific methods:
#'
#'   \describe{
#'      \item{\code{peaks}}{Returns a \code{list} of \linkS4class{GRangesList} 
#'                          of the per-condition original peaks used to build 
#'                          the object.}
#'      \item{\code{consensus}}{Returns a \linkS4class{GRangesList} of the 
#'                              per-condition consensus peaks.}
#'   }
#'
#' @return
#'   For \code{peaks}, a \code{list} of \linkS4class{GRangesList} of the 
#'   per-condition original peaks used to build the object. For 
#'   \code{consensus}, a \linkS4class{GRangesList} of the per-condition
#'   consensus peaks.
#'
#' @examples
#'   res = queryConsensusPeaks("22Rv1", "GRCh38", "CTCF")
#'   peaks(res)
#'   consensus(res)
#'
#' @name ENCODEBindingConsensus-class
#' @rdname ENCODEBindingConsensus-class
#' @export
setClass("ENCODEBindingConsensus",
         slots=list(peaks="list", 
                    consensus="GRangesList",
                    consensus_threshold="numeric"),
         contains="ENCODESummary")

#' @rdname ENCODEBindingConsensus-class
#' @export
setMethod("names<-",
          c(x="ENCODEBindingConsensus", value="character"),
          function(x, value) {
            names(x@peaks) <- value
            names(x@consensus) <- value
            methods::callNextMethod()
          })

#' @docType methods
#' @rdname ENCODEBindingConsensus-class
#' @export
setGeneric("peaks", function(x) standardGeneric("peaks"))

#' @rdname ENCODEBindingConsensus-class
#' @aliases peaks,ENCODEBindingConsensus,ENCODEBindingConsensus-method
setMethod("peaks",
          c(x="ENCODEBindingConsensus"),
          function(x) {
            x@peaks
          })

#' @docType methods
#' @rdname ENCODEBindingConsensus-class
#' @export
setGeneric("consensus", function(x) standardGeneric("consensus"))

#' @rdname ENCODEBindingConsensus-class
#' @aliases consensus,ENCODEBindingConsensus,ENCODEBindingConsensus-method
setMethod("consensus",
          c(x="ENCODEBindingConsensus"),
          function(x) {
            x@consensus
          })

#' @rdname ENCODEBindingConsensus-class
#' @export
setMethod("show", "ENCODEBindingConsensus",
          function(object) {
            cat("An object of class ENCODEBindingConsensus.\n")
            methods::callNextMethod()
            cat("\nConsensus regions:\n")
            print(consensus(object))
          })

#' ENCODEExpressionSummary summarize means of expression across ENCODE files.
#'
#' ENCODEExpressionSummary objects represent means (or medians) of expression
#' levels across multiple replicate samples, split by arbitrary metadata 
#' columns. They can be constructed using the \link{queryGeneExpression},
#' \link{queryTranscriptExpression} and \link{buildExpressionSummary} functions.
#'
#' @slot raw_data A list of data-frames containing the full raw data of each
#'                of the downloaded ENCODE files.
#' @slot metric A character giving the regular expression used to extract 
#'              expression metrics from the ENCODE files.
#' @slot metric_data A \link{data.frame} of the per-condition metric values.
#' @slot expression_type The type of expression which is being reported, either 
#'                       gene or transcripts.
#'
#' @param x The \linkS4class{ENCODESummary} object.
#' @param object The \linkS4class{ENCODESummary} object.
#' @param value The new names for the elements of the 
#'              \linkS4class{ENCODESummary} object.
#'
#' @section Methods:
#'
#'   ENCODEExpressionSummary object can be accessed through the methods from the 
#'   ENCODESummary class, as well as ENCODEBindingConsensus-specific methods:
#'
#'   \describe{
#'      \item{\code{raw_data}}{Returns a \code{list} of \linkS4class{GRangesList} 
#'                             of the per-condition original expression tables
#'                             used to build the object.}
#'      \item{\code{metric}}{Returns the regular expression used to select the 
#'                           column of metric values from the ENCODE files.}
#'      \item{\code{metric_data}}{Returns a \link{data.frame} of the 
#'                                per-condition metric values.}
#'   }
#'
#' @return
#'   For \code{raw_data}, a \code{list} of \linkS4class{GRangesList} 
#'   of the per-condition original expression tables used to build the object.
#'   For \code{metric}, the regular expression used to select the 
#'   column of metric values from the ENCODE files. For \code{metric_data},
#'   a \link{data.frame} of the per-condition metric values.
#'
#' @examples
#'   res = queryGeneExpression("bone marrow")
#'   raw_data(res)
#'   metric(res)
#'   metric_data(res)
#'
#' @name ENCODEExpressionSummary-class
#' @rdname ENCODEExpressionSummary-class
#' @export
setClass("ENCODEExpressionSummary",
         slots=list(raw_data="list", 
                    metric="character",
                    metric_data="data.frame",
                    expression_type="character"),
         contains="ENCODESummary")

#' @rdname ENCODEExpressionSummary-class
setMethod("names<-",
          c(x="ENCODEExpressionSummary", value="character"),
          function(x, value) {
            names(raw_data) <- value
            colnames(x@metric_data) <- c("id", value)
            methods::callNextMethod()
          })
         
#' @docType methods
#' @rdname ENCODEExpressionSummary-class
#' @export         
setGeneric("metric_data", function(x) standardGeneric("metric_data"))

#' @rdname ENCODEExpressionSummary-class
#' @aliases metric_data,ENCODEBindingConsensus,ENCODEBindingConsensus-method
setMethod("metric_data",
          c(x="ENCODEExpressionSummary"),
          function(x) {
            x@metric_data
          })         

#' @docType methods
#' @rdname ENCODEExpressionSummary-class
#' @export          
setGeneric("metric", function(x) standardGeneric("metric"))

#' @rdname ENCODEExpressionSummary-class
#' @aliases metric,ENCODEBindingConsensus,ENCODEBindingConsensus-method
setMethod("metric",
          c(x="ENCODEExpressionSummary"),
          function(x) {
            x@metric
          })     

#' @docType methods
#' @rdname ENCODEExpressionSummary-class
#' @export          
setGeneric("raw_data", function(x) standardGeneric("raw_data"))

#' @rdname ENCODEExpressionSummary-class
#' @aliases raw_data,ENCODEBindingConsensus,ENCODEBindingConsensus-method
setMethod("raw_data",
          c(x="ENCODEExpressionSummary"),
          function(x) {
            x@raw_data
          })

#' @rdname ENCODEExpressionSummary-class
#' @export
setMethod("show", "ENCODEExpressionSummary",
          function(object) {
            cat("An object of class ENCODEExpressionSummary.\n")
            methods::callNextMethod()
            cat("\nSumarizing", nrow(metric_data(object)), object@expression_type, 
                "expression levels.\n")
          })          