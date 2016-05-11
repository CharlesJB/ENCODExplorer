test.column_character <- function(){
  
  input <-c("a","b","c")
  df <- data.frame(a=input,stringsAsFactors = FALSE)
  obs <- ENCODExplorer:::clean_column("a",df)
  exp<-input
  checkIdentical(obs,exp)
  
  
}