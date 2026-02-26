options(width=120, digits=8, scipen=9, show.signif.stars=FALSE)
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
nReplicationsPerCondition = 30
nConditions = conditionInformation(arrayNumber=1, nReplicationsPerCondition=nReplicationsPerCondition)$nConditions
nReplications = nConditions * nReplicationsPerCondition



current_wd = getwd()
result_dir = file.path(current_wd, "results")
if (!dir.exists(result_dir)) dir.create(result_dir, recursive = TRUE)

# Run Analysis--------------------------------#
arrayNumber = 570
for(arrayNumber in 1:nReplications){

  conditions = conditionInformation(arrayNumber = arrayNumber, nReplicationsPerCondition=nReplicationsPerCondition)

  # 1. Prepare Data ------------------------------------------------------------#
  all_datasets = prepare_ml_datasets(
    full_data = itemDataF,
    target_variable = conditions$target,
    predictor_vars = predictor_vars,
    testSplit = conditions$testSplit,
    seed = arrayNumber
  )

  # frequency table of train_y by quantile
  quantiles = quantile(all_datasets$train$pct, probs = seq(0, 1, by = 0.25), na.rm = TRUE)
  groups_bf = cut(all_datasets$train$pct, breaks = quantiles, include.lowest = TRUE, right = FALSE)
  n_bf = as.data.frame(table(groups_bf))


  # 2. Data Augmentation -------------------------------------------------------#
  train_balanced = data_augmentation(
    data = all_datasets,
    augMethod = conditions$augMethod,
    nMultiple = 3,
    nTarget = n_bf[1,2]
    )

  # frequency table of train_balanced_y by quantile
  groups_af = cut(train_balanced$pct, breaks = quantiles, include.lowest = TRUE, right = FALSE)
  n_af = as.data.frame(table(groups_af))
  quantile_n = data.frame(
    Interval = n_bf$groups_bf,
    N_before = n_bf$Freq,
    N_after  = n_af$Freq
  )


  # 3. Train Data and Evaluate -------------------------------------------------#
  results = train_and_evaluate_model(
    train = train_balanced,
    test = all_datasets[["test"]],
    modelMethod=conditions$modelMethod,
    cvType = conditions$cvType,
    quantiles = quantiles
  )

  out_file = file.path(result_dir, paste0("cond_", arrayNumber, ".RData"))
  save(conditions, quantile_n, results, file = out_file)
}

