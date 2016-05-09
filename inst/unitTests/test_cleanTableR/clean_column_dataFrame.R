test_column_dataFrame <- function (){
  library(RUnit)
  df1 <- data.frame(a= c("a","b","c"),stringsAsFactors = FALSE)
  input <-data.frame(df1)
  obs <-clean_column("a",input)
  exp <- c("a","b","c")
  
  res<-checkIdentical(obs,exp)
  print(res)
  
}