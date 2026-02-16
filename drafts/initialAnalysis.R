devtools::load_all(".")

# import data
itemInfo = readxl::read_excel("1. (CASMA)초3수학진단향상도 (1).xlsx", skip =1, col_names =TRUE)


# ------------------------------------------------------------------------------
# 1. item expected probability based on ICC
#-------------------------------------------------------------------------------
# extract a and b
a = itemInfo$`변별도(변환 후)`
b = itemInfo$`난이도(변환 후)`

# theta quadrature points
theta = seq(-4, 4, by=0.2)

# calculate weight for each theta
prob = dnorm(theta, 0, 1)
weight = prob/sum(prob)

# get P(Y=1|theta) for every items
itemProbTheta = irt2PLprobability(a = a, b = b , theta = theta)

# calculated expected probability for all items
expectedProb = NULL
item=1
for(item in 1:length(a)){
  itemExpectedProb = 100*sum(itemProbTheta[,item]*weight)
  expectedProb = c(expectedProb, itemExpectedProb)
}

# # export
# write.csv(expectedProb, "expectedProb.csv")


#-------------------------------------------------------------------------------
# 2. Assumption check
#-------------------------------------------------------------------------------
# import data
itemInfoSub = readxl::read_excel("[AeKyong] Item Feature Coding.xlsx", skip =1, col_names =TRUE)
itemInfoSub = as.data.frame(itemInfoSub)

# remove special characters ( ) / , in variable name
colnames(itemInfoSub) = gsub("[()\\/\\ \\,]", "", colnames(itemInfoSub))


assmpCheck(variableName = "종류_", data = itemInfoSub)
assmpCheck(variableName = "구성요소_",  data = itemInfoSub)
assmpCheck(variableName = "도형제시_",  data = itemInfoSub)
assmpCheck(variableName = "단위_",  data = itemInfoSub)
assmpCheck(variableName = "자료_",  data = itemInfoSub)
assmpCheck(variableName = "자료해석_",  data = itemInfoSub)


#-------------------------------------------------------------------------------
# 3. Embedded Feature Selection
#-------------------------------------------------------------------------------
library(caret)
library(readxl)
library(writexl)
ml = function(removedVariables, dependentVariable, method){

  #############################################
  # prepare data -----------------------------#
  #############################################

  # import data
  itemFeature = readxl::read_excel("초3수학진단향상도_Item Feature Coding.xlsx", skip =1, col_names =TRUE, sheet = "Coding")

  # remove special characters ( ) / , in variable name
  colnames(itemFeature) = gsub("[()\\/\\ \\,]", "", colnames(itemFeature))

  # variables for prediction
  variables = colnames(itemFeature)[which(!colnames(itemFeature) %in% removedVariables)]

  # data for prediction
  predData = itemFeature[,variables]

  # criterion for train/test data
  testType = itemFeature$검사형

  # select train/test data based on test type
  trainData = predData[which(testType %in% c("22G", "23G", "24G",
                                             "A01", "A02", "A03",
                                             "B01", "B02")),]

  testData = predData[which(testType %in% c("25G")),]


  #############################################
  # train/test  ------------------------------#
  #############################################
  set.seed(3042)
  trControl = trainControl(method = "cv",
                           number = 3,
                           p=.8)
  # training
  fit_model = train(as.formula(paste(dependentVariable, "~ .")),
                    data = trainData,
                    method = method,
                    trControl = trControl
  )
# browser()
  # testing
  pred = predict(fit_model, testData)

  # compare true probability with predicted
  compare.true.pred = cbind(testData[,dependentVariable], pred, testData[,dependentVariable]-pred)
  colnames(compare.true.pred) = c("true", "predicted", "difference")

  #############################################
  # coefficient
  #############################################
  if(method == "glmnet"){
    coef = coef(fit_model$finalModel, s = fit_model$bestTune$lambda)
    coef_matrix = as.matrix(coef)

    coef_df <- data.frame(
      Variable = rownames(coef_matrix),
      Coefficient = coef_matrix[, "s1"]  # Assuming 's1' is the column name
    )
  } else {
    coef_df = NULL
  }


  #############################################
  # check importance of variables-------------#
  #############################################

  if(method == "glmnet"){
    # Calculate variable importance
    importance = varImp(fit_model, scale = FALSE)

    # All variables with importance
    imp_df = importance$importance
    imp_df$Variable = rownames(imp_df)

    # Select to top N variables
    top_vars = imp_df[order(imp_df$Overall, decreasing = TRUE), ][1:118,c("Variable", "Overall")]

  }else{
    top_vars = NULL
  }


  return(list(
    fit_model = fit_model,
    coef = coef_df,
    top_vars = top_vars,
    compare = compare.true.pred)
  )
}



# ---------------------------------------------------------------------------#
# svm linear ----------------------------------------------------------------#
# ---------------------------------------------------------------------------#
# library(kernlab)
library(ksvm)
# full data
fit.svm.full = ml(
  removedVariables = c("학년", "검사형", "문항번호", "문항ID", "성취기준",
                       "정답률변환전", "난이도변환전", "변별도변환전",
                       "난이도변환후", "변별도변환후",
                       "문항유형_선택형", "답항개수_4지"
  ),
  dependentVariable = "정답률변환후",
  method = "svmRadial"
)

writeLines(capture.output(fit.svm.full$fit_model), "4. fit.svm.full_model fit.txt")

# selected data
fit.svm.selected = ml(
  removedVariables = c("학년", "검사형", "문항번호", "문항ID", "성취기준",
                       "정답률변환전", "난이도변환전", "변별도변환전",
                       "난이도변환후", "변별도변환후",
                       "문항유형_선택형", "답항개수_4지",
                       "문항세부유형_부정형",
                       "문항세부유형_최선답형",
                       "계산복잡정도_측정단위간받아올림",
                       "계산복잡정도_받아내림없음",
                       "종류_선분",
                       "종류_직각삼각형",
                       "종류_직사각형",
                       "구성요소_각",
                       "도형제시_원의개수2개",
                       "도형제시_원의개수4개",
                       "단위_길이mm",
                       "단위_길이cm",
                       "단위_무게t",
                       "자료_항목3가지",
                       "자료_세로항목2가지"),
  dependentVariable = "정답률변환후",
  method = "svmRadial"
)

writeLines(capture.output(fit.svm.selected$fit_model), "4. fit.svm.selected_model fit.txt")


# ---------------------------------------------------------------------------#
# glmnet --------------------------------------------------------------------#
# ---------------------------------------------------------------------------#

# full data
fit.glmnet.full = ml(
  removedVariables = c("학년", "검사형", "문항번호", "문항ID", "성취기준",
                       "정답률변환전", "난이도변환전", "변별도변환전",
                       "난이도변환후", "변별도변환후",
                       "문항유형_선택형", "답항개수_4지"
  ),
  dependentVariable = "정답률변환후",
  method = "glmnet"
)

writeLines(capture.output(fit.glmnet.full$fit_model), "4. fit.glmnet.full.result_model fit.txt")


# selected data
fit.glmnet.selected = ml(
  removedVariables = c("학년", "검사형", "문항번호", "문항ID", "성취기준",
                       "정답률변환전", "난이도변환전", "변별도변환전",
                       "난이도변환후", "변별도변환후",
                       "문항유형_선택형", "답항개수_4지",
                       "문항세부유형_부정형",
                       "문항세부유형_최선답형",
                       "계산복잡정도_측정단위간받아올림",
                       "계산복잡정도_받아내림없음",
                       "종류_선분",
                       "종류_직각삼각형",
                       "종류_직사각형",
                       "구성요소_각",
                       "도형제시_원의개수2개",
                       "도형제시_원의개수4개",
                       "단위_길이mm",
                       "단위_길이cm",
                       "단위_무게t",
                       "자료_항목3가지",
                       "자료_세로항목2가지"),
  dependentVariable = "정답률변환후",
  method = "glmnet"
)

writeLines(capture.output(fit.glmnet.selected$fit_model), "4. fit.glmnet.selected_model fit.txt")


# export : compare prediction result
compare.predictions = cbind(
  fit.svm.full$compare,
  fit.svm.selected$compare,
  fit.glmnet.full$compare,
  fit.glmnet.selected$compare
)

colnames(compare.predictions) = c("svm.full.true","svm.full.predicted", "svm.full.diff",
                                  "svm.selected.true","svm.selected.predicted", "svm.selected.diff",
                                  "glmnet.full.true","glmnet.full.predicted", "glmnet.full.diff",
                                  "glmnet.selected.true","glmnet.selected.predicted", "glmnet.selected.diff")

write_xlsx(compare.predictions,
           "4. prediction comparision.xlsx",
           col_names = TRUE)



# export : coefficient
write_xlsx(fit.glmnet.full$coef, "4. fit.glmnet.full_coefficients.xlsx")
write_xlsx(fit.glmnet.selected$coef, "4. fit.glmnet.selected_coefficients.xlsx")


# export : variance importance
write_xlsx(fit.glmnet.selected$top_vars,
           "4. fit.glmnet.selected_variance importance.xlsx",
           col_names = TRUE)

write_xlsx(fit.glmnet.full$top_vars,
           "4. fit.glmnet.full_variance importance.xlsx",
           col_names = TRUE)



# ------------------------------------------------------------------------------
# 5. item expected probability based on ICC (Subpopulation)
#-------------------------------------------------------------------------------
# import data
itemInfo = readxl::read_excel("data/1. (CASMA)초3수학진단향상도 (1).xlsx", skip =1, col_names =TRUE)


a = itemInfo$`변별도(변환 후)`
b = itemInfo$`난이도(변환 후)`

# test type of A and G
testTypeAG = itemInfo[grep(c("A|G"), itemInfo$검사형), "검사형"]
testTypeAG = as.matrix(unique(testTypeAG))


csem2TestType = rep(NA, length(testTypeAG))
testType = 1
# sem for each test type
for(testType in 1:length(testTypeAG)){

  # item info data of each test type
  itemInfo.testType = itemInfo[itemInfo$검사형 == testTypeAG[testType,], ]

  # extract a and b
  a.testType = itemInfo.testType$`변별도(변환 후)`
  b.testType = itemInfo.testType$`난이도(변환 후)`

  # theta quadrature points
  thetaCut = -1.4

  # item information of A and G
  item.info =  irt2PLinformation(theta = thetaCut, a = a.testType, b = b.testType)

  # test info
  test.info = apply(item.info, 1, sum)

  # conditional SEM
  csem2 = 1/test.info

  csem2TestType[testType] = csem2
}

# average csem across test types
csemAvg = sqrt(mean(csem2TestType))

# cut point
thetaCut = -1.4

# theta quadrature of subpopulation
thetaSubQuad = seq(thetaCut - csemAvg, thetaCut + csemAvg, length.out = 41)

# theta quadrature of population
theta = seq(-4, 4, length.out = 41)

# probability of theta for subpopulation
prob = dnorm(thetaSubQuad, 0, 1)

# probability to weight
weight = prob/sum(prob)

# get P(Y=1|theta) of subpopulation
itemProbTheta = irt2PLprobability(a = a, b = b , theta = thetaSubQuad)

# calculated expected probability for all items
irtPCRsub = NULL
item=1
for(item in 1:length(a)){
  irtPCR = 100*sum(itemProbTheta[,item]*weight)
  irtPCRsub = c(irtPCRsub, irtPCR)
}

# export
write.csv(irtPCRsub, "irtPCRsub.csv")
