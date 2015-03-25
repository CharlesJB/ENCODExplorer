
print(paste0("ON EST LA : ",getwd()))

if(FALSE) {
  library( "RUnit" )
  library( "ENCODEdb" )
}

load(file = "extdata/test_matrices.rda") # matrices

limit = 4
searchTerm = "MCF7"
res = search(searchTerm, limit)
# test type de retour
checkTrue(expr = is.data.frame(res), msg = "res should be a data.frame")
# test taille de la df
checkEquals(nrow(res), limit,  
            msg = paste0("res should be a ",limit," rows data.frame"))

# test l'existance du champ accession
checkTrue(expr = !is.null(res$accession), 
          msg = "res should be get an accession entry")
# test l'absence de resultat pour requete farfelue
searchTerm = "AABBCC"
res = search(searchTerm, limit)

# test type de retour
checkTrue(expr = is.data.frame(res), msg = "res should be a data.frame")
# test taille de la df
checkEquals(nrow(res), 0,  
            msg = paste0("res should be a ",limit," rows data.frame"))

