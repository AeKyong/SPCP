rm(list=ls())
devtools::load_all(".")
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(ggpubr)
library(caret)

setwd("results")
# 결과를 담을 리스트 초기화
final_predictions_list <- list()
nReplicationsPerCondition <- 50

# 전체 실험 횟수 계산 (conditionInformation 함수 활용)
info_init <- conditionInformation(arrayNumber = 1, nReplicationsPerCondition = nReplicationsPerCondition)
nConditions <- info_init$nConditions
nReplications <- nConditions * nReplicationsPerCondition

for (i in 1:nReplications) {
  file_name <- paste0("cond_", i, ".RData")

  if (file.exists(file_name)) {
    print(file_name)
    load(file_name)

    # 2. 해당 회차의 실험 조건 정보 가져오기
    info <- conditionInformation(i, nReplicationsPerCondition = nReplicationsPerCondition)
    rep_idx <- ((i - 1) %% nReplicationsPerCondition) + 1

    # 3. 실험 조건 정보를 담은 기본 데이터프레임 생성
    temp_info <- data.frame(
      Target_Variable = info$target,
      AUG_Method = info$augMethod,
      Model = info$modelMethod,
      CV_Type = info$cvType,
      Split_Type = info$testSplit,
      Replication = rep_idx,
      # nTrain = n_train_balanced,
      stringsAsFactors = FALSE
    )

    # 4. 이미 계산되어 저장된 performance 데이터프레임 결합
    # results$performance가 1행짜리 데이터프레임이라고 가정합니다.
    temp_df <- cbind(temp_info, results$performance)

    final_predictions_list[[i]] <- temp_df
  }
}

# list to data frame
df <- dplyr::bind_rows(final_predictions_list)

# 6. Factor 설정 (시각화 시 순서 고정)
df$AUG_Method <- factor(df$AUG_Method,
                        levels = c("None", "RandOverRegress", "GaussNoiseRegress",
                                   "SmoteRegress", "SMOGNRegress", "WERCSRegress",
                                   "augMI", "augCAT"))
df$Model <- factor(df$Model, levels = c("Lasso", "xgbTree", "cubist"))




# T-test function
perform_t_tests <- function(data, metric) {
  # 해당 지표 존재 여부 확인
  if(!metric %in% names(data)) return(NULL)

  methods <- setdiff(unique(data$AUG_Method), "None")
  test_results <- list()

  # 기준군(None) 데이터 미리 추출
  group_none_raw <- data %>% filter(AUG_Method == "None") %>% pull(!!sym(metric))
  group_none <- group_none_raw[!is.na(group_none_raw)]

  for (m in methods) {
    group_comp_raw <- data %>% filter(AUG_Method == m) %>% pull(!!sym(metric))
    group_comp <- group_comp_raw[!is.na(group_comp_raw)]

    if(length(group_none) > 1 & length(group_comp) > 1) {
      # T-test 실행
      t_result <- t.test(group_none, group_comp)

      test_results[[m]] <- data.frame(
        Metric = metric,
        Method = m,
        # APA 스타일 핵심 지표
        M_None = mean(group_none),
        SD_None = sd(group_none),
        M_Comp = mean(group_comp),
        SD_Comp = sd(group_comp),
        t_stat = t_result$statistic, # t값
        df = t_result$parameter,      # 자유도
        p_value = t_result$p.value,   # p-value
        sig = ifelse(t_result$p.value < 0.001, "***",
                     ifelse(t_result$p.value < 0.01, "**",
                            ifelse(t_result$p.value < 0.05, "*", "ns")))
      )
    }
  }
  return(bind_rows(test_results))
}


# Box plot of RMSE and R-squared function
create_boxplot <- function(data, metric, title, y_label, y_limits = NULL) {
  # highlight_low 옵션과 상관없이 테두리 색상을 검정으로 통일
  border_color <- "black"

  p <- ggplot(data, aes(x = AUG_Method, y = .data[[metric]], fill = Model)) +
    geom_boxplot(outlier.shape = 21,
                 outlier.fill = "white",
                 outlier.size = 1.5,
                 alpha = 0.7,
                 color = border_color,
                 position = position_dodge(width = 0.8)) +
    labs(title = title, x = "Augmentation Method", y = y_label) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",          # 함수 기본값 하단 설정
          plot.title = element_text(face = "plain"), # 볼드체 제거 (일반체)
          panel.grid.minor = element_blank()) +
    scale_fill_brewer(palette = "Set2")

  if (!is.null(y_limits)) {
    p <- p + coord_cartesian(ylim = y_limits)
  }

  return(p)
}

# T-test result ------------------------#
metrics_test_rmse <- c("RMSE_Test", "0%-20%.RMSE", "20%-40%.RMSE", "40%-60%.RMSE", "60%-80%.RMSE", "80%-100%.RMSE")
t_test_rmse <- lapply(metrics_test_rmse, function(m) perform_t_tests(df, m))
write.csv(t_test_rmse, "table_t_test_rmse.csv")

metrics_test_mae <- c("MAE_Test", "0%-20%.MAE", "20%-40%.MAE", "40%-60%.MAE", "60%-80%.MAE", "80%-100%.MAE")
t_test_mae <- lapply(metrics_test_mae, function(m) perform_t_tests(df, m))
write.csv(t_test_mae, "table_t_test_mae.csv")

metrics_test_mape <- c("MAPE_Test", "0%-20%.MAPE", "20%-40%.MAPE", "40%-60%.MAPE", "60%-80%.MAPE", "80%-100%.MAPE")
t_test_mape <- lapply(metrics_test_mape, function(m) perform_t_tests(df, m))
write.csv(t_test_mape, "table_t_test_mape.csv")

metrics_test_r2 <- c("Rsquared_Test", "0%-20%.Rsquared", "20%-40%.Rsquared", "40%-60%.Rsquared", "60%-80%.Rsquared", "80%-100%.Rsquared")
t_test_r2 <- lapply(metrics_test_r2, function(m) perform_t_tests(df, m))
write.csv(t_test_r2, "table_t_test_r2.csv")

# Box plot ------------------------------#
rmseCols <- grep(".RMSE", names(df))
rmse_range = c(min(df[, rmseCols]), max(df[, rmseCols]))

maeCols <- grep(".MAE", names(df))
mae_range = c(min(df[, maeCols]), max(df[, maeCols]))

mapeCols <- grep(".MAPE", names(df))
mape_range = c(min(df[, mapeCols]), max(df[, mapeCols]))

r2Cols <- grep(".Rsquared", names(df))
r2_range = c(min(df[, r2Cols], na.rm=T), max(df[, r2Cols], na.rm=T))

plot_rmse_overall <- create_boxplot(df, "RMSE_Test", "Total Set", "RMSE", y_limits = rmse_range)
plot_rmse_0020 <- create_boxplot(df, "0%-20%.RMSE", "0-20%", "RMSE", y_limits = rmse_range)
plot_rmse_2040 <- create_boxplot(df, "20%-40%.RMSE", "20-40%", "RMSE", y_limits = rmse_range)
plot_rmse_4060 <- create_boxplot(df, "40%-60%.RMSE", "40-60%", "RMSE", y_limits = rmse_range)
plot_rmse_6080 <- create_boxplot(df, "60%-80%.RMSE", "60-80%", "RMSE", y_limits = rmse_range)
plot_rmse_80100 <- create_boxplot(df, "80%-100%.RMSE", "80-100%", "RMSE", y_limits = rmse_range)

plot_mae_overall <- create_boxplot(df, "MAE_Test", "Total Set", "MAE", y_limits = mae_range)
plot_mae_0020 <- create_boxplot(df, "0%-20%.MAE", "0-20%", "MAE", y_limits = mae_range)
plot_mae_2040 <- create_boxplot(df, "20%-40%.MAE", "20-40%", "MAE", y_limits = mae_range)
plot_mae_4060 <- create_boxplot(df, "40%-60%.MAE", "40-60%", "MAE", y_limits = mae_range)
plot_mae_6080 <- create_boxplot(df, "60%-80%.MAE", "60-80%", "MAE", y_limits = mae_range)
plot_mae_80100 <- create_boxplot(df, "80%-100%.MAE", "80-100%", "MAE", y_limits = mae_range)

plot_mape_overall <- create_boxplot(df, "MAPE_Test", "Total Set", "MAPE", y_limits = mape_range)
plot_mape_0020 <- create_boxplot(df, "0%-20%.MAPE", "0-20%", "MAPE", y_limits = mape_range)
plot_mape_2040 <- create_boxplot(df, "20%-40%.MAPE", "20-40%", "MAPE", y_limits = mape_range)
plot_mape_4060 <- create_boxplot(df, "40%-60%.MAPE", "40-60%", "MAPE", y_limits = mape_range)
plot_mape_6080 <- create_boxplot(df, "60%-80%.MAPE", "60-80%", "MAPE", y_limits = mape_range)
plot_mape_80100 <- create_boxplot(df, "80%-100%.MAPE", "80-100%", "MAPE", y_limits = mape_range)

plot_r2_overall <- create_boxplot(df, "Rsquared_Test", "Total", "R-Squared", y_limits = r2_range)
plot_r2_0020 <- create_boxplot(df, "0%-20%.Rsquared", "0-20%", "R-Squared", y_limits = r2_range)
plot_r2_2040 <- create_boxplot(df, "20%-40%.Rsquared", "20-40%", "R-Squared", y_limits = r2_range)
plot_r2_4060 <- create_boxplot(df, "40%-60%.Rsquared", "40-60%", "R-Squared", y_limits = r2_range)
plot_r2_6080 <- create_boxplot(df, "60%-80%.Rsquared", "60-80%", "R-Squared", y_limits = r2_range)
plot_r2_80100 <- create_boxplot(df, "80%-100%.Rsquared", "80-100%", "R-Squared", y_limits = r2_range)

combined_rmse <- (
  (plot_rmse_overall + plot_rmse_0020) /
  (plot_rmse_2040 + plot_rmse_4060) /
  (plot_rmse_6080 + plot_rmse_80100)
  ) +
  plot_layout(guides = 'collect') & # 'collect'로 범례를 하나로 모음
  theme(legend.position = "bottom")   # '&'를 사용하여 전체 레이아웃의 하단으로 강제 고정

combined_mae <- (
  (plot_mae_overall + plot_mae_0020) /
  (plot_mae_2040 + plot_mae_4060) /
  (plot_mae_6080 + plot_mae_80100)
) +
  plot_layout(guides = 'collect') & # 'collect'로 범례를 하나로 모음
  theme(legend.position = "bottom")

combined_mape <- (
  (plot_mape_overall + plot_mape_0020) /
  (plot_mape_2040 + plot_mape_4060) /
  (plot_mape_6080 + plot_mape_80100)
) +
  plot_layout(guides = 'collect') & # 'collect'로 범례를 하나로 모음
  theme(legend.position = "bottom")

combined_r2 <-  (
  (plot_r2_overall + plot_r2_0020) /
    (plot_r2_2040 + plot_r2_4060) /
    (plot_r2_6080 + plot_r2_80100)
) +
  plot_layout(guides = 'collect') &
  theme(legend.position = "bottom")

print(combined_rmse)
print(combined_mae)
print(combined_mape)
print(combined_r2)

ggsave(
  filename = "plot_RMSE.png",
  plot = combined_rmse,    # 위에서 만든 결합 객체 명시
  width = 12,              # 두 그래프가 가로로 놓이므로 가로 길이를 충분히 확보
  height = 12,              # 범례가 아래에 있으므로 높이도 적절히 조절
  units = "in",
  dpi = 300                # 고해상도 출력
)

ggsave(
  filename = "plot_MAE.png",
  plot = combined_mae,    # 위에서 만든 결합 객체 명시
  width = 12,              # 두 그래프가 가로로 놓이므로 가로 길이를 충분히 확보
  height = 12,              # 범례가 아래에 있으므로 높이도 적절히 조절
  units = "in",
  dpi = 300                # 고해상도 출력
)

ggsave(
  filename = "plot_MAPE.png",
  plot = combined_mape,    # 위에서 만든 결합 객체 명시
  width = 12,              # 두 그래프가 가로로 놓이므로 가로 길이를 충분히 확보
  height = 12,              # 범례가 아래에 있으므로 높이도 적절히 조절
  units = "in",
  dpi = 300                # 고해상도 출력
)

ggsave(
  filename = "plot_R2.png",
  plot = combined_r2,    # 위에서 만든 결합 객체 명시
  width = 12,              # 두 그래프가 가로로 놓이므로 가로 길이를 충분히 확보
  height = 12,              # 범례가 아래에 있으므로 높이도 적절히 조절
  units = "in",
  dpi = 300                # 고해상도 출력
)
