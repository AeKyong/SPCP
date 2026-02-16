
rm(list=ls())
devtools::load_all()
# install_github("paobranco/UBL",ref="development")
# install.packages('tinytex')
# tinytex::install_tinytex()
# install.packages("automap", lib = my_lib, repos = "https://cloud.r-project.org/")

# library(ItemFeature)
library(reshape)
library(automap) # tar.gz
library(devtools)
library(UBL)
library(e1071)
library(randomForest)
library(caret)
library(miceadds)
library(readxl)
library(glmnet)

# Prepare Data --------------------------------#
# load data
itemData = readxl::read_excel("data/math_g3_feature.xlsx", skip = 1, col_names =TRUE, sheet = "Coding")

# save original variable names
originalVarName = names(itemData)

# rename the variables
varNames.info = c("grade","testType", "itemNum", "itemID","standard",
                  "pctCTT","b.bf","a.bf","pctIRT","b.af","a.af", "pctSEM")
varNames.predictors = c(paste0("v", 1:(ncol(itemData)-length(varNames.info))))

colnames(itemData) = c(varNames.info, varNames.predictors)

# select data of predictor variables
predictors = itemData[,varNames.predictors]

# combine information  and predictor data
itemDataF = as.data.frame(cbind(itemData[,varNames.info], predictors))

# save final predictor variable names
predictor_vars = names(predictors)

# Set conditions--------------------------------#
nReplicationsPerCondition = 50
nConditions = conditionInformation(arrayNumber=1, nReplicationsPerCondition=nReplicationsPerCondition)$nConditions
nReplications = nConditions * nReplicationsPerCondition



current_wd = getwd()
result_dir = file.path(current_wd, "results")
if (!dir.exists(result_dir)) dir.create(result_dir, recursive = TRUE)

# Run Analysis--------------------------------#
arrayNumber = 22
for(arrayNumber in 1:nReplications){

  conditions = conditionInformation(arrayNumber = arrayNumber, nReplicationsPerCondition=nReplicationsPerCondition)

  # 1. Prepare Data -----------------------#
  all_datasets = prepare_ml_datasets(
    full_data = itemDataF,
    target_variable = conditions$target,
    predictor_vars = predictor_vars,
    testSplit = conditions$testSplit,
    seed = arrayNumber
  )

  # check the number of train_y by each interval
  train_y = all_datasets$train$pct
  quantiles = quantile(train_y, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
  intervals = names(quantiles)
  n_train = list()
  i=1
  for(i in 1:(length(intervals)-1)){
    lb = quantiles[i]
    ub = quantiles[i+1]
    idx = which(train_y >= lb & train_y < ub)
    n_idx = length(idx)
    n_train[[paste0(intervals[i], "-", intervals[i+1], "_N_bf")]] = n_idx
  }
  n_train_df = as.data.frame(do.call(cbind, n_train))
  quantile_n = cbind(as.data.frame(t(quantiles)), n_train_df)




  # 2. Data Augmentation ----------------------#
  train_balanced = data_augmentation(
    data = all_datasets,
    augMethod = conditions$augMethod,
    nSyn = (nrow(itemDataF) * 2)
  )

  # check the number of train_y by each interval after balancing
  train_balanced_y = train_balanced$pct
  n_train_balanced = list()
  i=1
  for(i in 1:(length(intervals)-1)){
    lb = quantiles[i]
    ub = quantiles[i+1]
    idx = which(train_balanced_y >= lb & train_balanced_y < ub)
    n_idx = length(idx)
    n_train_balanced[[paste0(intervals[i], "-", intervals[i+1], "_N_af")]] = n_idx
  }
  n_train_balanced_df = as.data.frame(do.call(cbind, n_train_balanced))
  quantile_n = cbind(quantile_n, n_train_balanced_df)




  # 3. Train Data and Evaluate ----------------------#
  results = train_and_evaluate_model(
    train = train_balanced,
    test = all_datasets[["test"]],
    modelMethod=conditions$modelMethod,
    cvType = conditions$cvType
  )

  out_file = file.path(result_dir, paste0("cond_", arrayNumber, ".RData"))
  save(conditions, quantile_n, results, file = out_file)
}

