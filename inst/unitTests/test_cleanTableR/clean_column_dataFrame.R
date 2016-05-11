test.column_dataFrame <- function (){

  df1 <- data.frame(a= c("a","b","c"),stringsAsFactors = FALSE)
  input <-data.frame(df1)
  obs <-ENCODExplorer:::clean_column("a",input)
  exp <- c("a","b","c")
  
  res<-checkIdentical(obs,exp)
  print(res)
  
}