#' ENCODEBindingConsensus objects represents consensus peaks derived from a 
#' set of ENCODE files.
#'
#' @slot metadata A list of data-frames representing the ENCODE metadata
#'                of the files used to build the per-condition consensus.
#' @slot peaks The per-condition original peaks used to build the consensus.
#' @slot consensus The per-condition consensus peaks.
#'
#' @name ENCODEBindingConsensus-class
#' @rdname ENCODEBindingConsensus-class
#' @export
setClass("ENCODEBindingConsensus",
         slots=list(metadata="list",
                    peaks="list", 
                    consensus="GRangesList",
                    consensus_threshold="numeric"))

#' Returns the names of the elements of a \linkS4class{ENCODEBindingConsensus} 
#' object. 
#'
#' @param x The \linkS4class{ENCODEBindingConsensus} object.
#' @return The names of the elements in \code{x}.
setMethod("names",
          c(x="ENCODEBindingConsensus"),
          function(x) {
            names(x@consensus)
          })

#' Set names of the elements of a \linkS4class{ENCODEBindingConsensus} object. 
#'
#' @param x The \linkS4class{ENCODEBindingConsensus} object.
#' @param value The new names for the elements of the 
#'              \linkS4class{ENCODEBindingConsensus} object.
setMethod("names<-",
          c(x="ENCODEBindingConsensus", value="character"),
          function(x, value) {
            names(x@metadata) <- value
            names(x@consensus) <- value
            names(x@peaks) <- value
            x
          })

#' Returns the number of elements of a \linkS4class{ENCODEBindingConsensus}
#' object. 
#'
#' @param x The \linkS4class{ENCODEBindingConsensus} object.
#' @return The number of elements in \code{x}.        
setMethod("length",
          c(x="ENCODEBindingConsensus"),
          function(x) {
            length(x@consensus)
          })   

setGeneric("metadata", function(x, ...) standardGeneric("metadata"))

#' Returns a list of per-condition metadata of the ENCODE files used to build
#' the \linkS4class{ENCODEBindingConsensus} object.
#'
#' @param x The \linkS4class{ENCODEBindingConsensus} object.
#' @return A \code{list} of per-condition metadata of the ENCODE files used to 
#' build the \linkS4class{ENCODEBindingConsensus} object.
#' @export
setMethod("metadata",
          c(x="ENCODEBindingConsensus"),
          function(x) {
            x@metadata
          })

setGeneric("peaks", function(x, ...) standardGeneric("peaks"))

#' Returns a \code{list} of \linkS4class{GRangesList} of the per-condition 
#' original peaks used to build the \linkS4class{ENCODEBindingConsensus} object.
#'
#' @param x The \linkS4class{ENCODEBindingConsensus} object.
#' @return A \code{list} of \linkS4class{GRangesList} of the per-condition  
#'         original peaks usedto build the \linkS4class{ENCODEBindingConsensus} 
#'         object.
#' @export
setMethod("peaks",
          c(x="ENCODEBindingConsensus"),
          function(x) {
            x@peaks
          })

setGeneric("consensus", function(x, ...) standardGeneric("consensus"))

#' Returns a \linkS4class{GRangesList} of the per-condition consensus peaks.
#'
#' @param x The \linkS4class{ENCODEBindingConsensus} object.
#' @return A \linkS4class{GRangesList} of the per-condition consensus peaks.
#' @export
setMethod("consensus",
          c(x="ENCODEBindingConsensus"),
          function(x) {
            x@consensus
          })

# Make sure a given column from query_results has only one unique value.
verify_unique <- function(query_results, col_name, label) {
    values = unique(query_results[[col_name]])
    if(length(values) != 1) {
        warning("Merging results with more than one listed ", label, ": (", 
                paste0(values, collapse=", "), ").\n",
                "Are you sure these can be combined?")
    }
}

# Given a metadata data-frame and a vector of column names (split_by),
# builds a partition of rows based on all possible combinations
# of the columns identified by split_by.
#' importFrom data.table rbindlist
split_by_metadata = function(metadata, split_by) {
    if(!all(split_by %in% colnames(metadata))) {
        stop("split_by must be a character vector of ",
             "query_results column names.")
    }

    # Determine all possible values for the split_by columns.
    split_by_list = as.list(split_by)
    names(split_by_list) = split_by
    possible_values = lapply(split_by_list, function(x) {unique(metadata[[x]])})
    
    # Determine all possible combinations of the split_by column values.
    combinations = expand.grid(possible_values)

    # Split rows by iterating over value combinations.
    out_subsets=list()
    new_metadata_list = list()
    partition = rep(NA, nrow(metadata))
    for(i in seq_len(nrow(combinations))) {
        # Select the columns where all values match the current combination.
        selected_subset = TRUE
        for(j in seq_len(ncol(combinations))) {
            col_name = colnames(combinations)[j]
            col_value = combinations[i,j]
            if(!is.na(col_value)) {
                selected_subset = selected_subset & (metadata[[col_name]] == col_value) & !is.na(metadata[[col_name]])
            } else {
                selected_subset = selected_subset & is.na(metadata[[col_name]])
            }
        }
        
        # If at least one row is selected, generate a name and assign it.
        if(sum(selected_subset, na.rm=TRUE) > 0) {
            region_name = paste(unlist(lapply(combinations[i, ], as.character)), collapse=";")
            out_subsets[[region_name]] = selected_subset
            partition[selected_subset] = i
            
            # We'll store the combination values for later use.
            new_metadata_list[[region_name]] = combinations[i,, drop=FALSE]
        }
    }
    
    # Concatenate the new metadata.
    new_metadata=data.table::rbindlist(new_metadata_list, use.names=TRUE)
    new_metadata$split_regions = names(new_metadata_list)

    # Return the results.
    return(list(Indices=out_subsets, Metadata=new_metadata, Partition=partition))        
}

# Makes sure the provided query_results can be used to build a consensus of 
# binding sites by checking column names and values.
validate_query_results_for_consensus <- function(query_results, split_by) {
    # Make sure query_results is a valid output from queryEncode, or at
    # least similar enough to be processed by downloadEncode.
    if(!is.data.frame(query_results)) {
        stop("query_results does not seem to be a data.frame")
    }
    
    if(!("file_accession" %in% colnames(query_results))) {
        stop("query_results does not seem to be valid. The file_accession column is missing.")
    }
    
    if(!("href" %in% colnames(query_results))) {
        stop("query_results does not seem to be valid. The href column is missing.")
    }

    # Make sure the specified split_by occur in query_results.
    if(!all(split_by %in% colnames(query_results))) {
        stop("The given split_by columns do not appear in query_results.")
    }
                                    
    # Check to see if the passed-in query results make sense in the context of
    # building consensus binding sites.
    verify_unique(query_results, "target", "antibody")
    verify_unique(query_results, "assembly", "assembly")
    verify_unique(query_results, "file_type", "file type")
}

validate_query_results_for_consensus_chip <- function(query_results, split_by) {
    validate_query_results_for_consensus(query_results, split_by)
    
    if(!all(query_results$file_type %in% c("bed narrowPeak", "bed broadPeak"))) {
        stop("Only narrowPeak and broadPeak files can be used to build a",
             "binding consensus.")
    }
}

validate_query_results_for_consensus_rna <- function(query_results, split_by) {
    validate_query_results_for_consensus(query_results, split_by)
    
    if(!all(query_results$file_type %in% c("tsv"))) {
        stop("Only tsv files can be used to build an expression consensus")
    }
}


#' Calculates the consensus peaks defined by the results of a previously
#' completed ENCODE query.
#'
#' This function takes the result of a previous call to 
#' \code{\link{queryEncode}}, splits the contained peak files by conditions (as
#' specified by the \code{split_by} argument), then builds consensus peaks for 
#' each condition.
#'
#' @param query_results A data.table returned by \code{\link{queryEncode}} or 
#'                      \code{\link{queryEncodeGeneric}}.
#' @param split_by A vector of column names from query_results that will be used
#'                 to split the consensus binding sites according to condition.
#'                 If \code{NULL}, all elements of query_results are used in the
#'                 same consensus calculation.
#' @param consensus_threshold A numeric value between 0 and 1, indicating the
#'                            proportion of peak files in which a peak must
#'                            appear for it to be included within the consensus.
#' @param temp_dir The path to a directory where peak files will be 
#'                 downloaded.
#' @param diagnostic_dir The path to a directory where information about the
#'                       peaks in individual files will be reported.
#' @param force A logical indicating whether already present files be 
#'              redownloaded.
#' @return An object of class \linkS4class{ENCODEBindingConsensus}.
#' @importFrom rtracklayer import
#' @importFrom GenomicRanges GRangesList
#' @importFrom GenomicOperations GenomicOverlaps
#' @importFrom GenomicOperations plot_venn
#' @importFrom GenomicOperations combined_regions
#' @export
buildConsensusPeaks <- function(query_results, split_by=NULL,
                                consensus_threshold=1,
                                temp_dir=".", diagnostic_dir=NULL,
                                force=FALSE) {
    validate_query_results_for_consensus_chip(query_results, split_by)
    
    dir.create(temp_dir, recursive=TRUE, showWarnings=FALSE)
    if(!is.null(diagnostic_dir)) {
        dir.create(diagnostic_dir, recursive=TRUE, showWarnings=FALSE)
    }
    
    # Download and import the peak files.
    downloaded_files = downloadEncode(query_results, dir=temp_dir, force=force)
    names(downloaded_files) = gsub(".*\\/(.*).bed.gz", "\\1", downloaded_files)

    file_format = gsub("bed ", "", unique(query_results$file_type))
    all_peaks = lapply(downloaded_files, rtracklayer::import, format=file_format)
    all_peaks = GenomicRanges::GRangesList(all_peaks)
    
    # Determine how to split files according to split_by.
    if(is.null(split_by)) {
        split_indices = list(All=rep(TRUE, nrow(query_results)))
    } else {
        split_indices = split_by_metadata(query_results, split_by)$Indices
    }
    
    # Split imported peaks and metadata.
    metadata = list()
    peaks = list()
    for(i in names(split_indices)) {
        metadata[[i]] = query_results[split_indices[[i]]]
        peaks[[i]] = all_peaks[metadata[[i]]$file_accession]
    }
    
    # Build consensus for each condition.
    consensus = list()
    for(i in names(peaks)) {
        overlap_obj = GenomicOperations::GenomicOverlaps(peaks[[i]])

#        if(!is.null(diagnostic_dir)) {
#            # Turn off VennDiagram logging.
#            futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
#            
#            pdf(file.path(diagnostic_dir, "Venn diagrams of peak calls", paste0(i, ".pdf")))
#            GenomicOperations::plot_venn(overlap_obj)        
#            dev.off()
#        }

        consensus[[i]] = consensus_regions(overlap_obj, consensus_threshold)
    }
    
    methods::new("ENCODEBindingConsensus",
                 metadata=metadata,
                 peaks=peaks, 
                 consensus=GenomicRanges::GRangesList(consensus),
                 consensus_threshold=consensus_threshold)
}

filter_results_by_file_type = function(query_results) {
    n_broad = sum(query_results$file_type=="bed broadPeak")
    n_narrow = sum(query_results$file_type=="bed narrowPeak")
    chosen_file_type = "bed narrowPeak"
    if(n_broad == 0 && n_narrow == 0) {
        warning("No peak files found for the specified query.")
    } else if(n_broad == n_narrow) {
        warning("Both broad and narrow peaks are present.",
                "Defaulting to using narrow peaks.")
    } else if(n_broad > n_narrow) {
        chosen_file_type = "bed broadPeak"
    } 
    
    query_results[query_results$file_type == chosen_file_type,]
}

filter_results_by_output_type = function(query_results) {
    unique_output = unique(query_results$output_type)
    if(length(unique_output) > 1) {
        prefered_output = c("optimal idr thresholded peaks",
                            "conservative idr thresholded peaks",
                            "peaks",
                            "replicated peaks",
                            "stable peaks")
                           
        output_matches = which(prefered_output %in% unique_output)
        if(length(output_matches) > 0) {
            best_output = prefered_output[output_matches[1]]
            query_results = query_results[query_results$output_type == best_output]
        } else {
            warning("Multiple output types were detected, and we could not choose one automatically.")
        }
    }
    
    query_results
}


#' Queries ENCODE for consensus peaks.
#'
#' Queries the ENCODE metadata to determine which peak files exists for the 
#' \code{target} protein in the \code{biosample_name} biosample for the 
#' \code{assembly} genomic assembly, then builds per-condition (as determined 
#' by the \code{treatment} column and its adjuncts) consensus peaks.
#'
#' If you wish to have more control over the files used to build the consensus,
#' use \code{\link{buildConsensusPeaks}}.
#'
#' @param biosample_name The cell-line/tissue for which consensus peaks should 
#'                       be queried.
#' @param assembly The target genomic assembly.
#' @param target The target protein.
#' @return An object of class \linkS4class{ENCODEBindingConsensus}.
#' @seealso \code{\link{buildConsensusPeaks}}
#' @importFrom rtracklayer import
#' @importFrom GenomicRanges GRangesList
#' @importFrom GenomicOperations GenomicOverlaps
#' @importFrom GenomicOperations plot_venn
#' @importFrom GenomicOperations combined_regions
#' @export
queryConsensusPeaks <- function(biosample_name, assembly, target) {
    query_results = queryEncodeGeneric(biosample_name=biosample_name, 
                                       assembly=assembly,
                                       target=target,
                                       assay="ChIP-seq",
                                       file_status="released")
    
    # Filter the results so we only keep "final" peaks.
    query_results = filter_results_by_file_type(query_results)
    query_results = filter_results_by_output_type(query_results)

    if(nrow(query_results)==0) {
        warning("No results found for the specified query.")
        return(NULL)    
    }
    
    default_split_by = c("treatment", 
                         "treatment_amount", "treatment_amount_unit",
                         "treatment_duration", "treatment_duration_unit")
    
    res = buildConsensusPeaks(query_results, default_split_by)
    if(length(res) == 1) {
        names(res) = "All"
    }
    
    return(res)
}

#' Queries and returns consensus peaks for all available targets of a given 
#' biosample_name.
#'
#' This utility function calls \link{queryConsensusPeaks} in a loop over all
#' available targets for a given biosample_name, and returns the results in a 
#' list.
#'
#' @param biosample_name The cell-line/tissue for which consensus peaks should 
#'                       be queried.
#' @param assembly The target genomic assembly.

#' @return A list of \linkS4class{ENCODEBindingConsensus}, one per available 
#'         target.
#' @seealso \code{\link{buildConsensusPeaks}}, \code{\link{queryConsensusPeaks}}
#' @export
queryConsensusPeaksAll <- function(biosample_name, assembly) {
    query_results = queryEncodeGeneric(biosample_name=biosample_name, 
                                       assembly=assembly, assay="ChIP-seq")
    
    all_targets = unique(query_results$target)
    lapply(all_targets, function(x) {
        queryConsensusPeaks(biosample_name, assembly, x)
    })
}

selectAssembly <- function(input_assemblies) {
    prefered_assembly = c("GRCh38", "GRCh38-minimal", "hg19",
                          "mm10", "mm10-minimal", "mm9",
                          "dm6", "dm3",
                          "ce11", "ce10",
                          "J02459.1")

    if(all(is.na(input_assemblies))) {
        message("Only assembly NA was found. Selecting it.")
        return(NA)
    } else {
        assembly_matches = which(prefered_assembly %in% input_assemblies)
        
        matched_assemblies = prefered_assembly[assembly_matches]
        selected_assembly = matched_assemblies[1]
        message("Found the following assemblies: ", paste0(matched_assemblies, collapse=", "))
        message("Selecting ", selected_assembly)
        
        return(selected_assembly)
    }
}

selectRNASeqAssay <- function(input_assays) {
    prefered_assay = c("total RNA-seq",
                       "polyA RNA-seq",
                       "polyA depleted RNA-seq",
                       "single cell RNA-seq",
                       "small RNA-seq")
    
    assay_matches = which(prefered_assay %in% input_assays)
    matched_assays = prefered_assay[assay_matches]
    
    return(prefered_assay[assay_matches[1]])
}

queryExpressionGeneric <- function(biosample_name, level="gene quantifications",
                                   assay=NULL, assembly=NULL) {
    query_results = queryEncodeGeneric(biosample_name=biosample_name, 
                                       file_type="tsv", 
                                       file_status="released",
                                       output_type=level)
                                       
    if(is.null(assembly)) {
        assembly = selectAssembly(query_results$assembly)
    }
    
    if(is.na(assembly)) {
        query_results = query_results[is.na(query_results$assembly),]
    } else {
        # assembly is a column name. data.table will interpret an "assembly"
        # variable as refering to the column, so the condition becomes
        # tautological. We avoid this by renaming the assembly external 
        # variable.
        # We could use with=FALSE, but then we'd need to explicitly specify
        # the columns we want (j-argument).
        chosen_assembly=assembly
        query_results = query_results[query_results$assembly==chosen_assembly,]
    }
    
    if(is.null(assay)) {
        assay = selectRNASeqAssay(query_results$assay)
    }
    # See note for chosen_assembly above.
    chosen_assay = assay
    query_results = query_results[query_results$assay==chosen_assay,]
    
    default_split_by=c("dataset_description", "treatment", 
                       "treatment_amount", "treatment_amount_unit",
                       "treatment_duration", "treatment_duration_unit")
    
    return(buildExpressionMean(query_results, split_by=default_split_by))
}

queryGeneExpression <- function(biosample_name, assembly=NULL) {
    queryExpressionGeneric(biosample_name, "gene quantifications", assembly)
}

queryTranscriptExpression <- function(biosample_name, assembly=NULL) {
    queryExpressionGeneric(biosample_name, "transcript quantifications", assembly)
}

dtColumnSummary = function(dt_files, column_name, summary_method=mean) {
    apply(do.call(cbind, lapply(dt_files, '[[', column_name)), 1, summary_method)
}

buildExpressionMean <- function(query_results, split_by, 
                                temp_dir=".", force=FALSE) {
    validate_query_results_for_consensus_rna(query_results, split_by)

    dir.create(temp_dir, recursive=TRUE, showWarnings=FALSE)
    
    # Download and import the peak files.
    downloaded_files = downloadEncode(query_results, dir=temp_dir, force=force)
    names(downloaded_files) = gsub(".*\\/(.*).tsv", "\\1", downloaded_files)

    # Determine how to split files according to split_by.
    if(is.null(split_by)) {
        split_indices = list(All=rep(TRUE, nrow(query_results)))
    } else {
        split_indices = split_by_metadata(query_results, split_by)$Indices
    }

    dt_files = lapply(downloaded_files, read.table, sep="\t", header=TRUE, stringsAsFactors=FALSE)
    
    id_column = ifelse("transcript quantifications" %in% query_results$output_type,
                       "transcript_id.s.",
                       "gene_id")
                       
    # Make sure the id-columns are identical across all files.
    example_ids = dt_files[[1]][[id_column]]
    all_same_ids = lapply(dt_files, function(x) { all(x[[id_column]] == example_ids) })
    if(!all(unlist(all_same_ids))) {
        stop("Some quantification files have different gene/transcript ids.")
    }
    
    # Build per-condition
    metadata = list()
    raw_data = list()
    tpm = list()
    fpkm = list()
    for(i in names(split_indices)) {
        metadata[[i]] = query_results[split_indices[[i]]]
        raw_data[[i]] = dt_files[split_indices[[i]]]

        tpm[[i]] = dtColumnSummary(dt_files[split_indices[[i]]], 'TPM', mean)
        fpkm[[i]] = dtColumnSummary(dt_files[split_indices[[i]]], 'FPKM', mean)
    }

    tpm_df = cbind(data.frame(id=example_ids), tpm)
    fpkm_df = cbind(data.frame(id=example_ids), fpkm)
    
    return(list(Metadata=metadata, TPM=tpm_df, FPKM=fpkm_df))
}