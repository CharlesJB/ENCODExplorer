if(FALSE) {
  library( "RUnit" )
  library( "ENCODExplorer" )
}

load(file = "inst/extdata/test_rest_search_experiment.rda") #search_experiment
load(file = "inst/extdata/test_rest_search_dataset.rda") #search_dataset
load(file = "inst/extdata/test_matrices.rda") # matrices

res = getExperimentDetails(resultSet = search_experiment)
checkTrue(nrow(res) > 0, msg = "this function should return experiment data")
res = getExperimentDetails(resultSet = search_dataset)
checkTrue(nrow(res) == 0, msg = "this function shouldn't return any dataset data")
