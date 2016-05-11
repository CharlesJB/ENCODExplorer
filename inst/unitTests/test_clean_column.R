if(FALSE) {
  library( "RUnit" )
  library( "ENCODExplorer" )
}

test.column_character <- function() {
  input <- c("a", "b", "c")
  df <- data.frame(a=input,stringsAsFactors=FALSE)
  obs <- ENCODExplorer:::clean_column("a", df)
  exp <- input
  checkIdentical(obs,exp)
}

test.column_dataFrame <- function () {
    df1 <- data.frame(a=c("a", "b", "c"), stringsAsFactors=FALSE)
    input <- data.frame(df1)
    obs <- ENCODExplorer:::clean_column("a", input)
    exp <- c("a", "b", "c")
    res <- checkIdentical(obs, exp)
    print(res)
}

test.column_numeric <- function() {
    input <- c(11, 22, 33)
    exp <- input
    df <- data.frame(title=input, stringsAsFactors=FALSE)
    obs <- ENCODExplorer:::clean_column("title", df)
    checkIdentical(obs, exp)
}

test.column_list_character <- function() {
    a <- c("String#1")
    b <- c("String#2")
    the_list <- c(a, b)
    df <- data.frame(the_list, stringsAsFactors=FALSE)
    input <- data.frame(df, stringsAsFactors=FALSE)
    obs <- ENCODExplorer:::clean_column("the_list", input)
    exp <- the_list
    checkIdentical(obs, exp)
}

#' The data.frame ABC_subset.rda can be generate with :
#' ABC <- jsonlite::fromJSON("https://www.encodeproject.org/search/?searchTerm=
#' ABC+CHIP&format=json&limit=10")
#' ABC_graph <- ABC[["@graph"]]
#' ABC_subset <- ABC_graph[c(1, 10),]

test.column_list_dataFrame <- function () {
    load(file=system.file("extdata/ABC_subset.rda", package="ENCODExplorer"))
    obs <- ENCODExplorer:::clean_column("replicates", ABC_subset)
    exp <- data.frame(antibody.accession=c(NA, "ENCAB000ARB"), 
        library.biosample.age=c("14", "14"), library.biosample.age_units=
        c("year","year"), library.biosample.life_stage=c("child", "child"), 
        library.biosample.organism.scientific_name=
        c("Homo sapiens", "Homo sapiens"), stringsAsFactors=FALSE)
    checkIdentical(obs, exp)
}

#test.column_list_dataFrame_multipleValue <- function () {
#    load(file=system.file("extdata/a549_ref.rda", package="ENCODExplorer"))
#   the_list <- list(a549_ref)
#}
