test_colomn_character <- function(){
  library(RUnit)
  input <-c("a","b","c")
  df <- data.frame(a=input,stringsAsFactors = FALSE)
  obs <- clean_column("a",df)
  exp<-input
  checkIdentical(obs,exp)
  
  
}