customSummary <- function(data, lev = NULL, model = NULL) {

  # 수치적으로 안정적인 역로짓 함수 정의
  inv_logit <- function(logit) {
    # R의 exp() 한계치를 고려한 Capping
    max_exponent = log(.Machine$double.xmax) - 10
    logit_capped = pmax(-max_exponent, pmin(max_exponent, logit))
    p = 1 / (1 + exp(-logit_capped))
    return(p * 100)
  }

  # 튜닝 과정 중의 pred와 obs(로짓값)를 실제 %값으로 변환
  actual_pct <- inv_logit(data$obs)
  predict_pct <- inv_logit(data$pred)

  # 실제 %값을 기준으로 지표 계산
  rmse <- sqrt(mean((actual_pct - predict_pct)^2, na.rm = TRUE))
  mae  <- mean(abs(actual_pct - predict_pct), na.rm = TRUE)

  # R-squared 계산 (상관관계가 없는 경우 에러 방지)
  r2 <- tryCatch({
    cor(actual_pct, predict_pct, use = "complete.obs")^2
  }, error = function(e) NA)

  # 이전에 정의하신 mape 함수 호출
  mape_val <- mape(actual_pct, predict_pct)

  out <- c(
    RMSE = rmse,
    MAE = mae,
    Rsquared = r2,
    MAPE = mape_val
  )

  return(out)
}
