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

###############################################################################
# Function to load and query DNA binding protein data
###############################################################################
build_binding_consensus <- function(query_results, split_by,
                                    consensus_threshold=1,
                                    temp_dir=".", diagnostic_dir=NULL,
                                    force=FALSE) {
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
    
    if(!all(query_results$file_type %in% c("bed narrowPeak", "bed broadPeak"))) {
        stop("Only narrowPeak and broadPeak files can be used to build a",
             "binding consensus.")
    }
    
    dir.create(temp_dir, recursive=TRUE, showWarnings=FALSE)
    if(!is.null(diagnostic_dir)) {
        dir.create(diagnostic_dir, recursive=TRUE, showWarnings=FALSE)
    }
    
    # Download and import the peak files.
    downloaded_files = downloadEncode(query_results, dir=temp_dir, force=force)
    names(downloaded_files) = gsub(".*\\/(.*).bed.gz", "\\1", downloaded_files)

    file_format = gsub("bed ", "", unique(query_results$file_type))
    all_binding_sites = GRangesList(lapply(downloaded_files, 
                                           rtracklayer::import, 
                                           format=file_format))
    
    # Split files by split_by values.
    split_res = split_by_metadata(query_results, split_by)
    metadata = list()
    binding_sites = list()
    for(i in names(split_res$Indices)) {
        metadata[[i]] = query_results[split_res$Indices[[i]]]
        binding_sites[[i]] = all_binding_sites[metadata[[i]]$file_accession]
    }
    
    consensus = list()
    for(i in names(binding_sites)) {
        overlap_obj = GenomicOperations::GenomicOverlaps(binding_sites[[i]])

#        if(!is.null(diagnostic_dir)) {
#            # Turn off VennDiagram logging.
#            futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
#            
#            pdf(file.path(diagnostic_dir, "Venn diagrams of peak calls", paste0(i, ".pdf")))
#            GenomicOperations::plot_venn(overlap_obj)        
#            dev.off()
#        }

        proportions = rowSums(intersect_matrix(overlap_obj)) / length(overlap_obj)
        consensus[[i]] = combined_regions(overlap_obj)[proportions >= consensus_threshold]
    }
    
    return(list(Metadata=metadata, BindingSites=binding_sites, Consensus=consensus))
}
