#temporairement
#ABC <- jsonlite::fromJSON("https://www.encodeproject.org/search/?searchTerm=ABC+CHIP&format=json&limit=10")
#ABC_graph<-ABC[["@graph"]]
#ABC_subset <-ABC_graph[c(1,10),]


test.column_list_dataFrame <- function (){
  library(RUnit)
  load(file = system.file("extdata/ABC_subset.rda", package = "ENCODExplorer")) #loading ABC_graph_replicates

  obs <- ENCODExplorer:::clean_column("replicates",ABC_subset)
  exp <-data.frame(antibody.accession=c(NA,"ENCAB000ARB"),library.biosample.age=c("14","14"),
                   library.biosample.age_units=c("year","year"),library.biosample.life_stage=c("child","child"),
                   library.biosample.organism.scientific_name=c("Homo sapiens","Homo sapiens"),
                   stringsAsFactors = FALSE)
  
  checkIdentical(obs,exp)

  
}