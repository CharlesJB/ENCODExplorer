
if(FALSE) {
  library( "RUnit" )
  library( "ENCODExplorer" )
}

load(file = "inst/extdata/test_mouse_donor_df.rda") #mouse_donor_df

mouse_df_clean = clean_table(mouse_donor_df)

# test the column renaming
checkTrue(!("@id" %in% names(mouse_df_clean)), msg = "this function should rename @id in id")
checkTrue(("id" %in% names(mouse_df_clean)), msg = "this function should rename @id in id")

# test column cleaning
checkTrue(!("@type" %in% names(mouse_df_clean)), msg = "this function should remove @type")
checkTrue(!("references" %in% names(mouse_df_clean)), msg = "this function should remove empty column")

# check column formatting
alternate_access = strsplit(as.character(mouse_df_clean[23,]$alternate_accessions), split = ";", fixed = TRUE)[[1]]
checkEquals(length(alternate_access),3, msg = "this function should tranform list in single vector whom elements are separated by a ;")

