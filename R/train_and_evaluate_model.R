train_and_evaluate_model = function(train, test, modelMethod, cvType) {

  tune_grid = NULL
 if (modelMethod == "Ridge") {
   tune_grid = expand.grid(alpha=0, lambda=10^seq(-5,1,length=10))
   } else if (modelMethod == "Lasso") {
     tune_grid = expand.grid(alpha=1, lambda=10^seq(-5,1,length=10))
     } else if (modelMethod == "svmRadial") {
       tune_grid = expand.grid(sigma=c(0.01, 0.1), C=c(0.5, 1, 2))
       } else if (modelMethod == "rf") {
         tune_grid = expand.grid(mtry=c(2, 4, 6))
         } else if (modelMethod == "xgbTree") {
           tune_grid = expand.grid(
             nrounds = c(100, 1000),
             max_depth = c(3, 9),
             eta = c(0.01, 0.1),
             gamma = c(0, 0.1, 0.2),
             colsample_bytree = c(0.6, 0.8),
             min_child_weight = c(1, 5),
             subsample = c(0.7, 0.9)
             )
           } else if (modelMethod == "cubist") {
             tune_grid = expand.grid(committees=c(1, 10), neighbors=c(0, 5))
             }

  train_data = train
  train_x = train_data[,!names(train_data) %in% c("pct")]
  train_y = train_data[,names(train_data) %in% c("pct")]
  train_y_logit = logit(train_y)

  test_data = test
  test_x = test_data[,!names(test_data) %in% c("pct","itemID")]
  test_y = test_data[,names(test_data) %in% c("pct")]

  nzv_train_x = nearZeroVar(train_x, saveMetrics=FALSE)
  if (length(nzv_train_x) > 0) {
    train_x = train_x[,-nzv_train_x,drop=FALSE]
    if (nrow(test_x) > 0) {
      test_x = test_x[,-nzv_train_x,drop=FALSE]
    }
  }

  if (cvType == "Standard"){
    fold_indices = create_standard_folds(train_y, k=5, seed=123)
  } else if(cvType == "Stratified"){
    fold_indices = create_stratified_folds(train_y, k=5, seed=123)
  }
  index_out = lapply(fold_indices, function(x) setdiff(1:length(train_y), x))


  train_control = trainControl(
    method = "cv",
    number = length(fold_indices),
    index = fold_indices,
    indexOut = index_out,
    savePredictions = "final",
    verboseIter = TRUE,
    summaryFunction=customSummary,
    returnResamp = "all"
  )

  model = NULL
  model = train(
    x=train_x, y=train_y_logit, method=ifelse(modelMethod %in% c("Ridge", "Lasso"), "glmnet", modelMethod),
    trControl=train_control, tuneGrid=tune_grid, preProcess=c("center", "scale")
  )

  # if (is.null(model)) {
  #   # na_metrics = c(RMSE=NA_real_, MAE=NA_real_, Rsquared=NA_real_, MAPE=NA_real_)
  #   performance_results = data.frame(
  #     RMSE_Train=NA_real_, MAE_Train=NA_real_, Rsquared_Train=NA_real_, MAPE_Train=NA_real_,
  #     RMSE_Test=NA_real_, MAE_Test=NA_real_, Rsquared_Test=NA_real_, MAPE_Test=NA_real_)
  #   predictions_df = data.frame(
  #     item_id=character(0),
  #     Actual=numeric(0), Predicted=numeric(0), Difference=numeric(0))
  #   importance_df = data.frame(Feature=character(0), Importance=numeric(0))
  #   cv_metrics = data.frame(Fold=NA_real_, RMSE=NA_real_, MAE=NA_real_, Rsquared=NA_real_, MAPE=NA_real_)
  #   }
  #
  # pred_train_logit = predict(model, newdata=train_x)
  # pred_train = inv_logit(pred_train_logit)
  # actual_train = inv_logit(train_y_logit)
  # metrics_train = c(
  #   postResample(pred=pred_train, obs=actual_train),
  #   MAPE=mape(actual_train, pred_train)
  # )
  #
  # pred_test = NA
  # actual_test = NA
  # metrics_test = c(RMSE=NA_real_, MAE=NA_real_, Rsquared=NA_real_, MAPE=NA_real_)
  # predictions_df = data.frame(
  #   item_id=character(0), Actual=numeric(0), Predicted=numeric(0), Difference=numeric(0)
  #   )
  #
  # if (nrow(test_data) > 0 && ncol(test_x) > 0) {
  #   pred_test_logit = predict(model, newdata=test_x)
  #   pred_test = inv_logit(pred_test_logit)
  #   actual_test = test_y
  #   metrics_test = c(
  #     postResample(pred=pred_test, obs=actual_test),
  #     MAPE=mape(actual_test, pred_test)
  #   )
  #   predictions_df = data.frame(
  #     item_id=test_data$itemID,
  #     Actual=actual_test,
  #     Predicted=pred_test,
  #     Difference=(actual_test-pred_test)
  #   )
  # } else {
  #   message(paste("Test set for", modelMethod, "is empty or has no predictors, skipping test set evaluation."))
  # }
  #
  # performance_results = data.frame(
  #   RMSE_Train=metrics_train['RMSE'],
  #   MAE_Train=metrics_train['MAE'],
  #   Rsquared_Train=metrics_train['Rsquared'],
  #   MAPE_Train=metrics_train['MAPE'],
  #   RMSE_Test=metrics_test['RMSE'],
  #   MAE_Test=metrics_test['MAE'],
  #   Rsquared_Test=metrics_test['Rsquared'],
  #   MAPE_Test=metrics_test['MAPE']
  # )

  # 1. Train 성능
  pred_train = inv_logit(predict(model, newdata=train_x))
  actual_train = train_y
  metrics_train = c(postResample(pred=pred_train, obs=actual_train), MAPE=mape(actual_train, pred_train))

  # 2. Test 성능 (전체 및 그룹별)
  metrics_test = c(RMSE=NA, MAE=NA, Rsquared=NA, MAPE=NA)
  metrics_test_low = c(RMSE=NA, MAE=NA, Rsquared=NA, MAPE=NA) # 하위 그룹용 지표 초기화
  predictions_df = data.frame()
  n_test_list = list()
  if (nrow(test) > 0) {
    pred_test = inv_logit(predict(model, newdata=test_x))
    actual_test = test_y

    # [전체 테스트 지표]
    metrics_test = c(postResample(pred=pred_test, obs=actual_test), MAPE=mape(actual_test, pred_test))

    # metrics per quantile
    quantiles = quantile(actual_test, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
    intervals = names(quantiles)
    metrics_by_percentile = list()
    i=1
    for(i in 1:(length(intervals)-1)){
      lb = quantiles[i]
      ub = quantiles[i+1]
      idx = which(actual_test >= lb & actual_test < ub)
      n_idx = length(idx)
      if(length(idx)>0){
        pred_sub = pred_test[idx]
        actual_sub = actual_test[idx]

        interval_label = paste0(intervals[i], "-", intervals[i+1])

        metrics_sub = c(
          postResample(pred=pred_sub, obs=actual_sub),
          MAPE = mape(actual_sub, pred_sub),
          Count = length(idx)
        )
        metrics_by_percentile[[interval_label]] = metrics_sub
      } # if
      n_test_list[[paste0(intervals[i], "-", intervals[i+1])]] = n_idx
    }# for

    predictions_df = data.frame(item_id=test$itemID, Actual=actual_test, Predicted=pred_test, Difference=(actual_test-pred_test))
  }
  # browser()

  n_test = do.call(cbind, n_test_list)
  percentile_wide = data.frame()
  if (length(metrics_by_percentile) > 0) {
    # 리스트의 각 요소(구간별 벡터)를 하나의 긴 벡터로 풀기(unlist)
    flat_metrics = unlist(metrics_by_percentile)
    # 한 행짜리 데이터프레임으로 변환
    percentile_wide = as.data.frame(t(flat_metrics))

    # 컬럼명을 "0%-20%.RMSE"와 같은 형식으로 가독성 있게 정리 (선택 사항)
    # names(percentile_wide) <- gsub("\\.", "_", names(percentile_wide))
  }

  # --- 최종 결과 데이터프레임 구성 (한 개의 행) ---
  performance_results = data.frame(
    # [1] 전체 훈련/테스트 지표
    RMSE_Train = metrics_train['RMSE'],
    MAE_Train = metrics_train['MAE'],
    Rsquared_Train = metrics_train['Rsquared'],
    RMSE_Test = metrics_test['RMSE'],
    MAE_Test = metrics_test['MAE'],
    Rsquared_Test = metrics_test['Rsquared'],
    MAPE_Test = metrics_test['MAPE']
  )

  # [2] 구간별 지표(Wide Form)를 옆으로 결합
  if (nrow(percentile_wide) > 0) {
    performance_results = cbind(performance_results, percentile_wide)
  }


  # CV metrics
  cv_metrics = model$resample[, c("RMSE", "MAE", "Rsquared", "MAPE")]
  cv_metrics$Fold = paste0("Fold", seq_len(nrow(cv_metrics)))

  imp = tryCatch(varImp(model, scale=FALSE), error=function(e) NULL)
  importance_df = data.frame(Feature=character(0), Importance=numeric(0))
  if (!is.null(imp)) {
    importance_df_raw = imp$importance
    if (nrow(importance_df_raw) > 0) {
      importance_df = data.frame(
        Feature=rownames(importance_df_raw),
        Importance=importance_df_raw[,1]
      )
    }
  } else {
    if (modelMethod == "glmnet" && !is.null(model$finalModel)) {
      coefs = coef(model$finalModel, s=model$bestTune$lambda)
      if (is(coefs, "sparseMatrix")) {
        coefs_df = data.frame(Feature=rownames(coefs)[-1], Importance=abs(as.matrix(coefs)[-1,1]))
        importance_df = coefs_df[coefs_df$Importance > 0,]
      }
    }
  }

  return(list(
    n_test = n_test,
    performance=performance_results,
    predictions=predictions_df,
    importance=importance_df,
    cv_metrics=cv_metrics))
}
