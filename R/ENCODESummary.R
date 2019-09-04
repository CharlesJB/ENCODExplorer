#' ENCODESummary objects represents a summary derived from multiple ENCODE 
#' files.
#'
#' @slot files The path of the files used in this summary.
#' @slot file_metadata A list of data-frames representing the ENCODE metadata
#'                     of the files used to build the per-condition consensus.
#' @slot metadata A data-frame with the metadata of each element in the summary.
#'
#' @name ENCODESummary-class
#' @rdname ENCODESummary-class
#' @export
setClass("ENCODESummary",
         slots=list(files="character",
                    file_metadata="list",
                    metadata="data.frame"))

#' Returns the names of the elements of a \linkS4class{ENCODESummary} 
#' object. 
#'
#' @param x The \linkS4class{ENCODESummary} object.
#' @return The names of the elements in \code{x}.
setMethod("names",
          c(x="ENCODESummary"),
          function(x) {
            names(x@file_metadata)
          })

#' Set names of the elements of a \linkS4class{ENCODESummary} object. 
#'
#' @param x The \linkS4class{ENCODESummary} object.
#' @param value The new names for the elements of the 
#'              \linkS4class{ENCODESummary} object.
#' @return The renamed object.
setMethod("names<-",
          c(x="ENCODESummary", value="character"),
          function(x, value) {
            rownames(x@metadata) <- value
            names(x@file_metadata) <- value
            x
          })

#' Returns the number of elements of a \linkS4class{ENCODESummary}
#' object. 
#'
#' @param x The \linkS4class{ENCODESummary} object.
#' @return The number of elements in \code{x}.        
setMethod("length",
          c(x="ENCODESummary"),
          function(x) {
            length(x@file_metadata)
          }) 

#' Returns a data-frame list of the common per-condition metadata of the ENCODE 
#' files used to build the \linkS4class{ENCODESummary} object.
#'
#' @param x The \linkS4class{ENCODESummary} object.
#' @return A \code{data.frame} of the common per-condition metadata of the 
#' ENCODE files used to build the \linkS4class{ENCODESummary} object.
#' @docType methods
#' @rdname metadata-methods
#' @export
setGeneric("metadata", function(x) standardGeneric("metadata"))

#' @rdname metadata-methods
#' @aliases metadata,ENCODESummary,ENCODESummary-method
setMethod("metadata",
          c(x="ENCODESummary"),
          function(x) {
            x@metadata
          })

#' Returns a list of per-condition metadata of the ENCODE files used to build
#' the \linkS4class{ENCODESummary} object.
#'
#' @param x The \linkS4class{ENCODESummary} object.
#' @return A \code{list} of per-condition metadata of the ENCODE files used to 
#' build the \linkS4class{ENCODESummary} object.
#' @docType methods
#' @rdname file_metadata-methods
#' @export
setGeneric("file_metadata", function(x) standardGeneric("file_metadata"))

#' @rdname file_metadata-methods
#' @aliases file_metadata,ENCODESummary,ENCODESummary-method
setMethod("file_metadata",
          c(x="ENCODESummary"),
          function(x) {
            x@file_metadata
          })

#' Returns a list of per-condition metadata of the ENCODE files used to build
#' the \linkS4class{ENCODESummary} object.
#'
#' @param x The \linkS4class{ENCODESummary} object.
#' @return A \code{list} of per-condition metadata of the ENCODE files used to 
#'         build the \linkS4class{ENCODESummary} object.
#' @docType methods
#' @rdname files-methods
#' @export
setGeneric("files", function(x) standardGeneric("files"))

#' @rdname files-methods
#' @aliases files,ENCODESummary,ENCODESummary-method
setMethod("files",
          c(x="ENCODESummary"),
          function(x) {
            x@files
          })

#' Prints a summary of a \linkS4class{ENCODESummary} object.
#'
#' @param object The \linkS4class{ENCODESummary} object.
#' @return Nothing.
#' @export
setMethod("show", "ENCODESummary",
          function(object) {
            cat("Summarizing", length(files(object)), "ENCODE files into",
                length(object), "categories.\n\n")
            cat("Metadata:\n")
            print(metadata(object))
          })

#' ENCODEBindingConsensus objects represents consensus peaks derived from a 
#' set of ENCODE files.
#'
#' @slot peaks The per-condition original peaks used to build the consensus.
#' @slot consensus The per-condition consensus peaks.
#' @slot consensus_threshold The proportion of replicates which must bear
#'                           a specific peak for it to be added to the set of
#'                           consensus peaks.
#'
#' @name ENCODEBindingConsensus-class
#' @rdname ENCODEBindingConsensus-class
#' @export
setClass("ENCODEBindingConsensus",
         slots=list(peaks="list", 
                    consensus="GRangesList",
                    consensus_threshold="numeric"),
         contains="ENCODESummary")

#' Set names of the elements of a \linkS4class{ENCODEBindingConsensus} object. 
#'
#' @param x The \linkS4class{ENCODEBindingConsensus} object.
#' @param value The new names for the elements of the 
#'              \linkS4class{ENCODEBindingConsensus} object.
#' @return The renamed object.
setMethod("names<-",
          c(x="ENCODEBindingConsensus", value="character"),
          function(x, value) {
            names(x@peaks) <- value
            names(x@consensus) <- value
            methods::callNextMethod()
          })

#' Returns a \code{list} of \linkS4class{GRangesList} of the per-condition 
#' original peaks used to build the \linkS4class{ENCODEBindingConsensus} object.
#'
#' @param x The \linkS4class{ENCODEBindingConsensus} object.
#' @return A \code{list} of \linkS4class{GRangesList} of the per-condition  
#'         original peaks usedto build the \linkS4class{ENCODEBindingConsensus} 
#'         object.
#' @docType methods
#' @rdname peaks-methods
#' @export
setGeneric("peaks", function(x) standardGeneric("peaks"))

#' @rdname peaks-methods
#' @aliases peaks,ENCODEBindingConsensus,ENCODEBindingConsensus-method
setMethod("peaks",
          c(x="ENCODEBindingConsensus"),
          function(x) {
            x@peaks
          })

#' Returns a \linkS4class{GRangesList} of the per-condition consensus peaks.
#'
#' @param x The \linkS4class{ENCODEBindingConsensus} object.
#' @return A \linkS4class{GRangesList} of the per-condition consensus peaks.
#' @docType methods
#' @rdname consensus-methods
#' @export
setGeneric("consensus", function(x) standardGeneric("consensus"))

#' @rdname consensus-methods
#' @aliases consensus,ENCODEBindingConsensus,ENCODEBindingConsensus-method
setMethod("consensus",
          c(x="ENCODEBindingConsensus"),
          function(x) {
            x@consensus
          })

#' Prints a summary of a \linkS4class{ENCODEBindingConsensus} object.
#'
#' @param object The \linkS4class{ENCODEBindingConsensus} object.
#' @return Nothing.
#' @export
setMethod("show", "ENCODEBindingConsensus",
          function(object) {
            cat("An object of class ENCODEBindingConsensus.\n")
            methods::callNextMethod()
            cat("\nConsensus regions:\n")
            print(consensus(object))
          })

#' ENCODEExpressionSummary objects represents means of gene- or 
#' transcript-levels of expression across a set of ENCODE files.
#'
#' @slot raw_data A list of data-frames containing the full raw data of each
#'                of the downloaded ENCODE files.
#' @slot metric A character giving the regular expression used to extract 
#'              expression metrics from the ENCODE files.
#' @slot metric_data A \link{data.frame} of the per-condition metric values.
#' @slot expression_type The type of expression which is being reported, either 
#'                       gene or transcripts.
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

#' Set names of the elements of a \linkS4class{ENCODEExpressionSummary} object. 
#'
#' @param x The \linkS4class{ENCODEExpressionSummary} object.
#' @param value The new names for the elements of the 
#'              \linkS4class{ENCODEExpressionSummary} object.
#' @return The renamed object.
setMethod("names<-",
          c(x="ENCODEExpressionSummary", value="character"),
          function(x, value) {
            names(raw_data) <- value
            colnames(x@metric_data) <- c("id", value)
            methods::callNextMethod()
          })
         
#' Returns a \link{data.frame} of the per-condition metric values.
#'
#' @param x The \linkS4class{ENCODEExpressionSummary} object.
#' @return A \link{data.frame} of the per-condition metric values.
#' @docType methods
#' @rdname metric_data-methods
#' @export         
setGeneric("metric_data", function(x) standardGeneric("metric_data"))

#' @rdname metric_data-methods
#' @aliases metric_data,ENCODEBindingConsensus,ENCODEBindingConsensus-method
setMethod("metric_data",
          c(x="ENCODEExpressionSummary"),
          function(x) {
            x@metric_data
          })         

#' Returns the regular expression used to extract metric values from the 
#' ENCODE files.
#' 
#' @param x The \linkS4class{ENCODEExpressionSummary} object.
#' @return The regular expression used to extract metric values from the 
#' ENCODE files.
#'         
#' @docType methods
#' @rdname metric-methods
#' @export          
setGeneric("metric", function(x) standardGeneric("metric"))

#' @rdname metric-methods
#' @aliases metric,ENCODEBindingConsensus,ENCODEBindingConsensus-method
setMethod("metric",
          c(x="ENCODEExpressionSummary"),
          function(x) {
            x@metric
          })     

#' Returns the regular expression used to extract raw_data values from the 
#' ENCODE files.
#' 
#' @param x The \linkS4class{ENCODEExpressionSummary} object.
#' @return A list with one element per file-group containing a list of the 
#'         raw imported ENCODE files.
#'         
#' @docType methods
#' @rdname raw_data-methods
#' @export          
setGeneric("raw_data", function(x) standardGeneric("raw_data"))

#' @rdname raw_data-methods
#' @aliases raw_data,ENCODEBindingConsensus,ENCODEBindingConsensus-method
setMethod("raw_data",
          c(x="ENCODEExpressionSummary"),
          function(x) {
            x@raw_data
          })

#' Prints a summary of a \linkS4class{ENCODEExpressionSummary} object.
#'
#' @param object The \linkS4class{ENCODEExpressionSummary} object.
#' @return Nothing.
#' @export
setMethod("show", "ENCODEExpressionSummary",
          function(object) {
            cat("An object of class ENCODEExpressionSummary.\n")
            methods::callNextMethod()
            cat("\nSumarizing", nrow(metric_data(object)), object@expression_type, 
                "expression levels.\n")
          })          