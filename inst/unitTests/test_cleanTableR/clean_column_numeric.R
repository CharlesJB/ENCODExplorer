test_column_numeric <- function(){
  library(RUnit)
  input <- c(11,22,33)
  exp <- input
  df <- data.frame(title=input,stringsAsFactors = FALSE)
  obs <- clean_column("title",df)
  checkIdentical(obs,exp)
}