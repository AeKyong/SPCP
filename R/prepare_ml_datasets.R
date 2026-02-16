prepare_ml_datasets = function(full_data, target_variable, predictor_vars, testSplit, seed) {

  set.seed(seed)
  full_data_complete_target = full_data

  if(testSplit == "random"){
    train_index_random =  sample(1:nrow(full_data_complete_target), size=floor(0.9*nrow(full_data_complete_target)), replace=FALSE)
    train_data = full_data_complete_target[train_index_random, c("itemID", target_variable, predictor_vars)]
    test_data = full_data_complete_target[-train_index_random, c("itemID", target_variable, predictor_vars)]
    colnames(train_data) = colnames(test_data)= c("itemID", "pct", predictor_vars)
  } else if(testSplit == "25G"){
    train_data = full_data_complete_target[full_data_complete_target$testType != "25G",c("itemID", target_variable, predictor_vars)]
    test_data = full_data_complete_target[full_data_complete_target$testType == "25G",c("itemID", target_variable, predictor_vars)]
    colnames(train_data)= colnames(test_data)= c("itemID", "pct", predictor_vars)
  }

  return(list(
    train = train_data,
    test = test_data
  ))
}
