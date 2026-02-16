augNN = function(dat , target, predictors, nSyn, swap_rate=0.1){
# browser()
  nn_grid =  expand.grid(
    size = c(1, 3, 5),     # Number of neurons in the hidden layer
    decay = c(0.01, 0.1)  # Weight decay
  )

  predictors.str = paste(predictors, collapse = " + ")
  formula.str = paste(target, "~", predictors.str)
  model.formula = as.formula(formula.str)

  # run neuralnet
  fit = train(model.formula,
                 data = dat,
                 method = "nnet",
                 trControl = trainControl(method = "cv", number = 5),
                 preProcess = c("center", "scale"),
                 tuneGrid = nn_grid
                 )



  # generate X values
  idx = sample(1:nrow(dat), nSyn, replace = TRUE)
  xSyn = dat[idx, predictors]

  for(var in predictors){
    # swap_rate 비율만큼의 행을 선택
    swap_idx = which(runif(nSyn) < swap_rate)
    if(length(swap_idx) > 0){
      # 선택된 행의 해당 변수 값을 원본 데이터에서 다시 무작위로 가져옴
      xSyn[swap_idx, var] = sample(dat[, var], length(swap_idx), replace = TRUE)
    }
  }

  # generate Y values
  # 3. Y값 예측 (학습된 NN 모델 활용)
  yPred = predict(fit, newdata = xSyn)

  # 실제 모델의 잔차를 계산하여 랜덤하게 더해줌 (데이터의 분산을 복원)
  resids = dat[, target] - predict(fit, newdata = dat)
  ySyn = yPred + sample(resids, nSyn, replace = TRUE)

  # 정답률 범위(0~1) 제한
  ySyn = pmax(0, pmin(1, ySyn))

  # combine x matrix and y matrix
  syn.dat = cbind(ySyn, xSyn)
  colnames(syn.dat)[1] = "pct"

  df = rbind(dat, syn.dat)
  return(df)
}
