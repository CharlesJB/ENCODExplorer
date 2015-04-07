if(FALSE) {
  library( "RUnit" )
  library( "ENCODEdb" )
}

my.term = "mcf7"
my.transfo = "^m[ ,-]?c[ ,-]?f[ ,-]?7"

checkEquals(query_transform(my.term), my.transfo,
            msg = "1.Error in query transformation")

my.term = "mcf-7"
checkEquals(query_transform(my.term), my.transfo,
            msg = "2.Error in query transformation")

my.term = "mcf 7 "
checkEquals(query_transform(my.term), my.transfo,
            msg = "3.Error in query transformation")

my.term = "mcf 7"
checkEquals(query_transform(my.term), my.transfo,
            msg = "4.Error in query transformation")
