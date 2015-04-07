if(FALSE) {
  library( "RUnit" )
  library( "ENCODEdb" )
}

load(file = "inst/extdata/test_rest_search_experiment.rda") #search_experiment
load(file = "inst/extdata/test_rest_search_dataset.rda") #search_dataset
load(file = "inst/extdata/test_matrices.rda") # matrices

res = getDatasetDetails(resultSet = search_dataset)
checkTrue(nrow(res) > 0, msg = "this function should return dataset data")
res = getDatasetDetails(resultSet = search_experiment)
checkTrue(nrow(res) == 0, msg = "this function shouldn't return any experiment data")
