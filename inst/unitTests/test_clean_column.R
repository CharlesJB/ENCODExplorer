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

    load(file=system.file("extdata/ABC_df_lab.rda", package="ENCODExplorer"))
    stopifnot(is.data.frame(ABC_lab))
    stopifnot(is.data.frame(ABC_lab[,1]))
    obs <- ENCODExplorer:::clean_column("lab", ABC_lab)
    exp <- data.frame(title=c("Bradley Bernstein, Broad", "NA", "NA", "NA",
           "Richard Myers, HAIB", "Richard Myers, HAIB", 
           "John Stamatoyannopoulos, UW", "Richard Myers, HAIB",
           "Richard Myers, HAIB", "John Stamatoyannopoulos, UW"), 
           stringsAsFactors = FALSE)
    checkIdentical(exp, obs)
}

test.column_numeric <- function() {
    input <- c(11, 22, 33)
    exp <- input
    df <- data.frame(title=input, stringsAsFactors=FALSE)
    obs <- ENCODExplorer:::clean_column("title", df)
    checkIdentical(obs, exp)
}

test.column_integer <- function() {
    load(file=system.file("extdata/dataFrame_integer.rda", package="ENCODExplorer"))
    input <- dataFrame_integer
    stopifnot(is.data.frame(input))
    stopifnot(is.integer(input[[1]]))
    obs <- ENCODExplorer:::clean_column("x", input)
    exp <- as.integer(c(4, 1, 1, 1))
    checkIdentical(obs[1:4], exp)
}

test.column_list_character <- function() {
    
    load(file=system.file("extdata/ABC_list_char.rda", package="ENCODExplorer"))
    input <- ABC_list_char
    stopifnot(is.data.frame(ABC_list_char))
    stopifnot(is.list(ABC_list_char[,1]))
    stopifnot(all(sapply(ABC_list_char[,1],class) == "character" | (sapply
            (ABC_list_char[,1],is.null))))
    obs <- ENCODExplorer:::clean_column("@type", input)
    exp <- c("Experiment; Dataset; Item","AntibodyLot; Item")
    stopifnot(is.character(obs))
    checkIdentical(obs[1:2], exp)
}

#' The data.frame ABC_subset.rda can be generate with :
#' ABC <- jsonlite::fromJSON("https://www.encodeproject.org/search/?searchTerm=
#' ABC+CHIP&format=json&limit=10")
#' ABC_graph <- ABC[["@graph"]]
#' ABC_subset <- ABC_graph[c(1, 10),]

test.column_list_dataFrame <- function () {
    load(file=system.file("extdata/ABC_subset.rda", package="ENCODExplorer"))
    stopifnot(is.data.frame(ABC_subset))
    stopifnot(is.list(ABC_subset$replicates))
    stopifnot(all(sapply(ABC_subset$replicates,class) == "data.frame" | sapply
                  (ABC_subset$replicates, is.null)))
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
