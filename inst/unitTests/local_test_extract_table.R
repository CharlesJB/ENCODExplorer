if(FALSE) {
  library( "RUnit" )
  library( "ENCODExplorer" )
}

res = extract_table(type = "platform")
checkTrue(nrow(res) > 0, "this function should return an non empty data.frame")

res = extract_table(type = "test")
checkTrue(nrow(res) == 0, "this function should return an empty data.frame")