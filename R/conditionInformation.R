conditionInformation = function(arrayNumber, nReplicationsPerCondition) {

  # convert array number to condition number
  conditionNumber = floor((arrayNumber - 1)/nReplicationsPerCondition) + 1

  # create conditions list
  conditions = list(
    target = c(
      # "pctCTT",
      "pctIRT"
               ),
    testSplit = c(
      "random"
      # "25G"
      ),
    augMethod = c(
      "None",
      "RandOverRegress",  # can make pct<0 or pct>1. Need post-processing
      "GaussNoiseRegress", # same as above
      "SmoteRegress", # 1. change the value between (0,1) -> 0 or 1, 2. Treat them as a probability
      "SMOGNRegress", # same as above
      "WERCSRegress",#importance sampling
      "augMI",  # statistically strong
      "augCAT" # amplifying when sample size is small.
      # "augNN" # similar with Virtual Sample Generation(VSG). sample size is small, so R2 is too small. augMI or augCAT might be better .
      ),
    cvType = c(
      "Standard"
      # "Stratified"
      ),
    modelMethod = c(
      # "Ridge",
      "Lasso",
      # "rf",
      "xgbTree", #interaction of discrete X variables
      # "svmRadial",
      "cubist"
      )
  )

  # number of conditions
  nConditions = prod(unlist(lapply(X = conditions, FUN = length)))

  conditionsMatrix = matrix(NA, nrow = nConditions, ncol = length(conditions))
  colnames(conditionsMatrix) = names(conditions)

  cond = 1
  for (cond in 1:nConditions){
    conditionsMatrix[cond,] = dec2bin(
      decimal_number = cond - 1,
      nattributes = length(conditions),
      basevector = unlist(lapply(X = conditions, FUN = length))
    ) + 1
  }

  # populate condition values
  target = conditions$target[conditionsMatrix[conditionNumber,1]]
  testSplit = conditions$testSplit[conditionsMatrix[conditionNumber,2]]
  augMethod = conditions$augMethod[conditionsMatrix[conditionNumber,3]]
  cvType = conditions$cvType[conditionsMatrix[conditionNumber,4]]
  modelMethod =  conditions$modelMethod[conditionsMatrix[conditionNumber,5]]


  return(list(
    nConditions = nConditions, conditionsMatrix = conditionsMatrix,
    target = target, testSplit = testSplit, augMethod = augMethod, cvType = cvType, modelMethod = modelMethod)
    )

}

# 12/31/2025
# 1. This can be removed.
# # remove all 1 or 0 variable
# predictors = predictors[,which(apply(predictors, 2, sum)!= nrow(predictors))] # remove v1, v23
# predictors = predictors[,which(apply(predictors, 2, sum)!= 0)]

# Discussion
# 1/2
# If rare case is increased, overfitting is occurred.
