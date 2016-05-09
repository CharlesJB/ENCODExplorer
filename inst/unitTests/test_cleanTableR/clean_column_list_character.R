test_column_list_character <- function(){
  library(RUnit)
  a <- c("String#1")
  b <- c("String#2")
  the_list <- c(a,b)

  df <-data.frame(the_list,stringsAsFactors = FALSE)
  input <- data.frame(df,stringsAsFactors = FALSE)
  obs <- clean_column("the_list",input)
  exp <- the_list
  
  checkIdentical(obs,exp)
}