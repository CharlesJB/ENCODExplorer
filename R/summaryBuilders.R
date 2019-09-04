# Encapsulates teh common parts for consensus peaks and expression summaries.
buildConsensusCommon = function(query_results, split_by, temp_dir, force, 
                                extension, simplify) {
    validate_query_results_for_consensus(query_results, split_by)
    
    dir.create(temp_dir, recursive=TRUE, showWarnings=FALSE)
    
    # Download and import the peak files.
    downloaded_files = downloadEncode(query_results, dir=temp_dir, force=force)
    name_regex = paste0(".*\\/(.*)", extension)
    names(downloaded_files) = gsub(name_regex, "\\1", downloaded_files)

    # Determine how to split files according to split_by.
    if(is.null(split_by)) {
        split_indices = list(All=rep(TRUE, nrow(query_results)))
    } else {
        split_info = split_by_metadata(query_results, split_by, simplify)
        split_indices = split_info$Indices
    }

    file_metadata = lapply(split_indices, function(x) {
        query_results[x]
    })
    
    return(list(Files=downloaded_files, 
                Metadata=split_info$Metadata, 
                FileMetadata=file_metadata,
                Indices=split_info$Indices))
}

# Make sure a given column from query_results has only one unique value.
verify_unique <- function(query_results, col_name, label) {
    values = unique(query_results[[col_name]])
    if(length(values) != 1) {
        warning("Merging results with more than one listed ", label, ": (", 
                paste0(values, collapse=", "), ").\n",
                "Are you sure these can be combined?")
    }
}

split_by_combination = function(metadata, combinations, simplify) {
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
    if(!(simplify && ncol(new_metadata) == 1)) {
        new_metadata$split_group = names(new_metadata_list)
    }
    rownames(new_metadata) = names(new_metadata_list)

    # Return the results.
    return(list(Indices=out_subsets, Metadata=new_metadata, Partition=partition))
}


#' Given a metadata data-frame and a vector of column names (split_by),
#' builds a partition of rows based on all possible combinations
#' of the columns identified by split_by.
#'
#' @param metadata A data.frame with the metadata.
#' @param split_by The names of the columns in metadata according to which they 
#'        should be split.
#' @param simplify If TRUE, columns with no discriminatory power will be removed
#'        from the resulting Metadata. Furthermore, if there is only one 
#'        category left, it will be labeled "All".
#' @return A list with information about the new partition.
#' @keywords internal
#' importFrom data.table rbindlist
split_by_metadata = function(metadata, split_by, simplify=FALSE) {
    if(!all(split_by %in% colnames(metadata))) {
        stop("split_by must be a character vector of ",
             "query_results column names.")
    }

    # Determine all possible values for the split_by columns.
    split_by_list = as.list(split_by)
    names(split_by_list) = split_by
    possible_values = lapply(split_by_list, function(x) {unique(metadata[[x]])})
    
    if(simplify) {
        # Determine which columns provide no discriminating information.
        single_value = lapply(possible_values, function(x) {length(unique(x))==1})
        
        if(all(unlist(single_value))) {
            # If no columns provide discriminatory factors, we won't do 
            # any splitting and return a simple object where everything is part
            # of the "All" partition.
            all_indices = seq_len(nrow(metadata))
            all_metadata = do.call(cbind.data.frame, possible_values)
            rownames(all_metadata) = "All"
            single_partition = rep(1, nrow(metadata))
            return(list(Indices=list(All=all_indices), 
                        Metadata=all_metadata,
                        Partition=single_partition))        
        } else {
            # Otherwise, we'll remove the non-discriminatory columns from the 
            # output.
            possible_values = possible_values[!unlist(single_value)]
        }
    }
    
    # Determine all possible combinations of the split_by column values.
    combinations = expand.grid(possible_values)

    # Split rows by iterating over value combinations.
    return(split_by_combination(metadata, combinations, simplify))
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
#' @param simplify If TRUE, non-discriminatory columns are removed from the metadata,
#'           and if only one sample group is found, it is renamed "All".
#' @param temp_dir The path to a directory where peak files will be 
#'                 downloaded.
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
                                consensus_threshold=1, simplify=FALSE,
                                temp_dir=".", force=FALSE) {
    common = buildConsensusCommon(query_results, split_by, temp_dir, force, 
                                  ".bed.gz", simplify)
    validate_query_results_for_consensus_chip(query_results, split_by)
    
    file_format = gsub("bed ", "", unique(query_results$file_type))
    all_peaks = lapply(common$Files, rtracklayer::import, format=file_format)
    all_peaks = GenomicRanges::GRangesList(all_peaks)
    
    # Split imported peaks and metadata.
    peaks = lapply(common$FileMetadata, function(x) {
        all_peaks[x$file_accession]
    })
    
    # Build consensus for each condition.
    consensus = list()
    for(i in names(peaks)) {
        overlap_obj = GenomicOperations::GenomicOverlaps(peaks[[i]])
        consensus[[i]] = GenomicOperations::consensus_regions(overlap_obj, consensus_threshold)
    }
    
    methods::new("ENCODEBindingConsensus",
                 files=common$Files,
                 metadata=common$Metadata,
                 file_metadata=common$FileMetadata,
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

PREFERED_OUTPUT_TYPE = c("optimal idr thresholded peaks",
                         "conservative idr thresholded peaks",
                         "peaks",
                         "replicated peaks",
                         "stable peaks")

DEFAULT_SPLIT_BY = c("treatment", 
                     "treatment_amount", "treatment_amount_unit",
                     "treatment_duration", "treatment_duration_unit")

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
#' @param simplify If TRUE, non-discriminatory columns are removed from the metadata,
#'           and if only one sample group is found, it is renamed "All".
#' @param use_interactive If TRUE, the user will be prompted when ENCODExplorer 
#'                        must choose how to filter the available data.
#' @return An object of class \linkS4class{ENCODEBindingConsensus}.
#' @seealso \code{\link{buildConsensusPeaks}}
#' @importFrom rtracklayer import
#' @importFrom GenomicRanges GRangesList
#' @importFrom GenomicOperations GenomicOverlaps
#' @importFrom GenomicOperations plot_venn
#' @importFrom GenomicOperations combined_regions
#' @export
queryConsensusPeaks <- function(biosample_name, assembly, target, 
                                simplify=FALSE, use_interactive=FALSE) {
    query_results = queryEncodeGeneric(biosample_name=biosample_name, 
                                       assembly=assembly,
                                       target=target,
                                       assay="ChIP-seq",
                                       file_status="released")
    
    # Filter the results so we only keep "final" peaks.
    query_results = filter_results_by_file_type(query_results)
    query_results = filter_on_values(query_results, "output_type", NULL,
                                     PREFERED_OUTPUT_TYPE, use_interactive)

    if(nrow(query_results)==0) {
        warning("No results found for the specified query.")
        return(NULL)    
    }
    
    res = buildConsensusPeaks(query_results, DEFAULT_SPLIT_BY, 
                              simplify=simplify)
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

#' importFrom utils menu
choose_interactive_value = function(values, col_name, allow_all) {
    count_table = sort(table(values, useNA="ifany"))
    count_values = names(count_table)
    if("NA" %in% count_values) {
        count_values["NA"] = NA
    }        
    
    menu_prompt = paste0("Multiple values for ", col_name,
                         " found. Which one should we use?")
    if(allow_all) {
        menu_prompt = paste0(menu_prompt, " Enter 0 to select all.")
    }
    
    value_prompt = paste0(names(count_table), " (", count_table, " files)")                                 
    menu_choice = menu(value_prompt, title=menu_prompt)
    if(menu_choice==0 && !allow_all) {
        stop("You must make a valid choice.")
    } else if(menu_choice!=0) {
        chosen_value = count_values[menu_choice]
    } else {
        chosen_value = count_values
    }
    
    
    return(chosen_value)
}

choose_prefered_value = function(values, col_name, preference_order) {
    value_matches = which(preference_order %in% values)
    
    matched_values = preference_order[value_matches]
    if(length(matched_values)==0) {
        stop("We could not select a value for", col_name, "automatically.")
    }
    chosen_value = matched_values[1]
    message("Found the following ", col_name, ": ",
            paste0(unique(values), collapse=", "))
    message("Selecting ", chosen_value, ". To choose another ", col_name,
            ", specify it in the '", col_name, "', argument or set ",
            "use_interactive to TRUE.")
    
    return(chosen_value)
}

filter_on_values <- function(query_results, col_name, chosen_value,
                             preference_order, use_interactive=FALSE,
                             allow_all=FALSE) {
    stopifnot(col_name %in% colnames(query_results))
    values = query_results[[col_name]]

    if(is.null(chosen_value)) {
        # Deal with the edge case where there is only one value.
        if(length(unique(values)) == 1) {
            message("Only ", unique(values), " was found. Selecting it.")
            return(query_results)
        }
    
        # Count the unique values to report them to the user.
        if(use_interactive) {
            chosen_value = choose_interactive_value(values, col_name, allow_all)
        } else {
            chosen_value = choose_prefered_value(values, col_name, 
                                                 preference_order)
        }
    }

    query_results = query_results[values %in% chosen_value |
                                  (is.na(values) & any(is.na(chosen_value))),]

    
    return(query_results)
}

ASSEMBLY_PREFERENCE = c("GRCh38", "GRCh38-minimal", "hg19",
                        "mm10", "mm10-minimal", "mm9",
                        "dm6", "dm3",
                        "ce11", "ce10",
                        "J02459.1")

ASSAY_PREFERENCE = c("total RNA-seq",
                     "polyA RNA-seq",
                     "polyA depleted RNA-seq",
                     "single cell RNA-seq",
                     "small RNA-seq")

#' Queries and returns average expression levels for a given biosample_name.
#'
#' ENCODE files are automatically split by biosample_description (which will
#' separate samples from different cell fractions or sequencing methods) and by
#' the treatment columns.
#'
#' @param biosample_name The cell-line/tissue for which average expression 
#'                       levels should be queried.
#' @param level The type of expression level to summarize, either 
#'              "gene quantifications" or "transcript quantifications".
#' @param assay The assay type to summarize. If \code{NULL}, the most generic
#'              assay type is automatically selected.
#' @param assembly The target genomic assembly. If \code{NULL}, the most recent 
#'                 available assembly is selected.
#' @param simplify If TRUE, non-discriminatory columns are removed from the metadata,
#'           and if only one sample group is found, it is renamed "All".
#' @param use_interactive If TRUE, the user will be prompted to select prefered
#'                        metadata values when multiple possibilities are
#'                        available.
#' @return An object of class \linkS4class{ENCODEExpressionSummary}.
#' @seealso \code{\link{buildExpressionSummary}}, 
#'          \code{\link{queryGeneExpression}}
#' @export
queryExpressionGeneric <- function(biosample_name, level="gene quantifications",
                                   assay=NULL, assembly=NULL, simplify=TRUE,
                                   use_interactive=FALSE) {
    query_results = queryEncodeGeneric(biosample_name=biosample_name, 
                                       file_type="tsv", 
                                       file_status="released",
                                       output_type=level)

    query_results = filter_on_values(query_results, "assembly", assembly,
                                     ASSEMBLY_PREFERENCE, use_interactive)
                                     
    query_results = filter_on_values(query_results, "assay", assay,
                                     ASSAY_PREFERENCE, use_interactive)                                     

    if(use_interactive) {
        query_results = filter_on_values(query_results, "dataset_description", 
                                         NULL, ASSAY_PREFERENCE, 
                                         use_interactive, allow_all=TRUE)
    }

    split_by = DEFAULT_SPLIT_BY
    if(length(unique(query_results$dataset_description)) > 1) {
        split_by = c("dataset_description", split_by)
    }

    return(buildExpressionSummary(query_results, split_by=split_by, 
                                  simplify=simplify))
}

#' Queries and returns average gene expression level for a given biosample_name.
#'
#' ENCODE files are automatically split by biosample_description (which will
#' separate samples from different cell fractions or sequencing methods) and by
#' the treatment columns.
#' @param biosample_name The cell-line/tissue for which average expression 
#'                       levels should be queried.
#' @param assay The assay type to summarize. If \code{NULL}, the most generic
#'              assay type is automatically selected.
#' @param assembly The target genomic assembly. If \code{NULL}, the most recent 
#'                 available assembly is selected.
#' @param simplify If TRUE, non-discriminatory columns are removed from the metadata,
#'           and if only one sample group is found, it is renamed "All".
#' @param use_interactive If TRUE, the user will be prompted to select prefered
#'                        metadata values when multiple possibilities are
#'                        available.
#'                 available assembly is selected.
#' @return An object of class \linkS4class{ENCODEExpressionSummary}.
#' @seealso \code{\link{buildExpressionSummary}}, 
#'          \code{\link{queryTranscriptExpression}}
#' @export
queryGeneExpression <- function(biosample_name, assay=NULL, assembly=NULL, 
                                simplify=TRUE, use_interactive=FALSE) {
    queryExpressionGeneric(biosample_name, "gene quantifications", assay, 
                           assembly, simplify, use_interactive)
}

#' Queries and returns average transcript expression level for a given 
#' biosample_name.
#'
#' ENCODE files are automatically split by biosample_description (which will
#' separate samples from different cell fractions or sequencing methods) and by
#' the treatment columns.
#'
#' @param biosample_name The cell-line/tissue for which average expression 
#'                       levels should be queried.
#' @param assay The assay type to summarize. If \code{NULL}, the most generic
#'              assay type is automatically selected.
#' @param assembly The target genomic assembly. If \code{NULL}, the most recent 
#'                 available assembly is selected.
#' @param simplify If TRUE, non-discriminatory columns are removed from the metadata,
#'           and if only one sample group is found, it is renamed "All".
#' @param use_interactive If TRUE, the user will be prompted to select prefered
#'                        metadata values when multiple possibilities are
#'                        available.
#' @return An object of class \linkS4class{ENCODEExpressionSummary}.
#' @seealso \code{\link{buildExpressionSummary}}, 
#'          \code{\link{queryGeneExpression}}
#' @export
queryTranscriptExpression <- function(biosample_name, assay=NULL, assembly=NULL, 
                                      simplify=TRUE, use_interactive=FALSE) {
    queryExpressionGeneric(biosample_name, "transcript quantifications", 
                           assay, assembly, simplify, use_interactive)
}

dtColumnSummary = function(dt_files, column_name, summary_method=mean) {
    col_data = lapply(dt_files, function(x) {
        col_match = grepl(column_name, colnames(x))
        if(sum(col_match) == 0) {
            stop("Column not found!")
        } else if(sum(col_match) > 1) {
            stop("Multiple columns match the specification")
        } else {
            return(x[[which(col_match)]])
        }
    })
    apply(do.call(cbind, col_data), 1, summary_method)
}

select_metric = function(metric, dt_files) {
    available_columns = c("^TPM$", "^FPKM$", ".*featurecounts.*", metric)
    is_available = lapply(available_columns, function(x) {
        unlist(lapply(dt_files, function(y) {
            any(grepl(x, colnames(y)))
        }))
    })
    
    is_available_all = unlist(lapply(is_available, all))
    if(is.null(metric)) {
        if(!any(is_available_all)) {
            stop("No known metric column is available in all files.")
        } else {
            metric = available_columns[is_available_all][1]
        }
    } else {
        if(!is_available_all[metric]) {
            stop("The selected column is not available for all files.")
        }
    }
    
    return(metric)
}

#' Calculates average expression levels of the results of a previously
#' completed ENCODE query.
#'
#' This function takes the result of a previous call to 
#' \code{\link{queryEncode}}, splits the contained expression files by 
#' conditions (as specified by the \code{split_by} argument), then calculates
#' average expression levels for each condition.
#'
#' @param query_results A data.table returned by \code{\link{queryEncode}} or 
#'                      \code{\link{queryEncodeGeneric}}.
#' @param split_by A vector of column names from query_results that will be used
#'                 to split the average expression levels.
#'                 If \code{NULL}, all elements of query_results are used in the
#'                 same average expression calculation.
#' @param metric A regular expression, indicating which column from the ENCODE
#'               data must be extracted. If NULL, ENCODExplorer data 
#'               automatically detects and selects one of the TPM, FPKM or
#'               featurecount columns.
#' @param aggregate_function A function which takes a vector as input and 
#'                           returns a single value summarizing the whole. Used
#'                           to summarize expression metrics. 
#' @param simplify If TRUE, non-discriminatory columns are removed from the metadata,
#'           and if only one sample group is found, it is renamed "All".
#' @param temp_dir The path to a directory where peak files will be 
#'                 downloaded.
#' @param force A logical indicating whether already present files be 
#'              redownloaded.
#' @return An object of class \linkS4class{ENCODEExpressionSummary}.
#' @export
buildExpressionSummary <- function(query_results, split_by, metric=NULL,
                                   simplify=FALSE, aggregate_function=mean, 
                                   temp_dir=".", force=FALSE) {
    common = buildConsensusCommon(query_results, split_by, temp_dir, force, 
                                  ".tsv", simplify=simplify)
    validate_query_results_for_consensus_rna(query_results, split_by)

    dt_files = lapply(common$Files, utils::read.table, sep="\t", header=TRUE, 
                      stringsAsFactors=FALSE)
    
    expression_type = ifelse("transcript quantifications" %in% query_results$output_type,
                             "transcript", 
                             "gene")
    id_column = ifelse(expression_type=="transcript", "transcript_id.s.", "gene_id")
                       
    # Make sure the id-columns are identical across all files.
    example_ids = dt_files[[1]][[id_column]]
    all_same_ids = lapply(dt_files, function(x) { all(x[[id_column]] == example_ids) })
    if(!all(unlist(all_same_ids))) {
        stop("Some quantification files have different gene/transcript ids.")
    }
    
    metric = select_metric(metric, dt_files)
    
    # Build per-condition summaries
    raw_data = lapply(common$Indices, function(x) {dt_files[x]})
    expression_levels = lapply(common$Indices, function(x) {
        dtColumnSummary(dt_files[x], metric, aggregate_function)
    })
    
    methods::new("ENCODEExpressionSummary",
             files=common$Files,
             file_metadata=common$FileMetadata,
             metadata=common$Metadata,
             raw_data=raw_data,
             metric=metric,
             metric_data=cbind(data.frame(id=example_ids), expression_levels),
             expression_type=expression_type)
}