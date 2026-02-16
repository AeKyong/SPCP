# Load necessary libraries
# rm(list=ls())
library(dplyr)
library(ggplot2)
library(stringr)

# load performance file
performance = read.csv("4. performance.csv")

#-------------------------------------------------------------------------------
# anova(evaluation ~ Balance method)
#-------------------------------------------------------------------------------
aov_rmse = aov(RMSE_Test ~ Balance_Method, performance)
summary(aov_rmse)

aov_mae = aov(MAE_Test  ~ Balance_Method, performance)
summary(aov_mae)

aov_r2 = aov(Rsquared_Test ~ Balance_Method, performance)
summary(aov_r2)

aov_mape = aov(MAPE_Test ~ Balance_Method, performance)
summary(aov_mape)


#-------------------------------------------------------------------------------
# Summary Table
#-------------------------------------------------------------------------------

balance_order <- c("NA", "RO", "GN", "SR", "SG", "IS", "MI", "CT")
model_order <- c("Ridge", "Lasso", "SVR", "Random Forest", "XGBoost", "Cubist")

performance_summary <- performance %>%
  # 1. data cleaning and change character to numeric
  mutate(
    Balance_Method = ifelse(is.na(Balance_Method), "NA", Balance_Method),
    Target_Variable = str_remove(Target_Variable, "pct"),
    RMSE_Test = as.numeric(as.character(RMSE_Test)),
    MAE_Test = as.numeric(as.character(MAE_Test)),
    Rsquared_Test = as.numeric(as.character(Rsquared_Test)),
    MAPE_Test = as.numeric(as.character(MAPE_Test))
  ) %>%
  filter(!is.na(RMSE_Test)) %>%
  # 2. factor level
  mutate(
    Balance_Method = factor(Balance_Method, levels = balance_order),
    Model = factor(Model, levels = model_order)
  ) %>%
  # 3. facet label
  mutate(facet_label = paste(Target_Variable, Split_Type, sep = ", ")) %>%
  # 4. delete results of model which only NA has the result
  group_by(Model, Target_Variable, Split_Type, CV_Type) %>%
  filter(n() > 1) %>%
  ungroup()

#-------------------------------------------------------------------------------
# Plot: 5 facets (4 metrics by models)
#-------------------------------------------------------------------------------
unique_models <- unique(performance_summary$Model)

y_limits_rmse <- range(performance_summary$RMSE_Test, na.rm = TRUE)
y_limits_mae  <- range(performance_summary$MAE_Test, na.rm = TRUE)
y_limits_r2  <- range(performance_summary$Rsquared_Test, na.rm = TRUE)
y_limits_mape <- range(performance_summary$MAPE_Test, na.rm = TRUE)

for (current_model in unique_models) {

  plot_data <- performance_summary %>% filter(Model == current_model)

  p_rmse <- ggplot(plot_data, aes(x = Balance_Method, y = RMSE_Test, color = CV_Type, group = CV_Type)) +
    geom_line(linewidth=1) + geom_point(size=2.5) + facet_wrap(~ facet_label) +
    labs(x = NULL, y = "Average RMSE", color = "CV Type") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
    coord_cartesian(ylim = y_limits_rmse)

  p_mae <- ggplot(plot_data, aes(x = Balance_Method, y = MAE_Test, color = CV_Type, group = CV_Type)) +
    geom_line(linewidth=1) + geom_point(size=2.5) + facet_wrap(~ facet_label) +
    labs(x = NULL, y = "Average MAE", color = "CV Type") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
    coord_cartesian(ylim = y_limits_mae)

  p_r2 <- ggplot(plot_data, aes(x = Balance_Method, y = Rsquared_Test, color = CV_Type, group = CV_Type)) +
    geom_line(linewidth=1) + geom_point(size=2.5)+ facet_wrap(~ facet_label) +
    labs(x = NULL, y = "Average R-squared", color = "CV Type") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")+
    coord_cartesian(ylim = y_limits_r2)

  p_mape <- ggplot(plot_data, aes(x = Balance_Method, y = MAPE_Test, color = CV_Type, group = CV_Type)) +
    geom_line(linewidth=1) + geom_point(size=2.5) + facet_wrap(~ facet_label) +
    labs(x = "Augmentation Method", y = "Average MAPE", color = "CV Type") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
    coord_cartesian(ylim = y_limits_mape)

  ggsave(filename = paste0("6. plot_", current_model, "_RMSE.png"), plot = p_rmse, width = 8, height = 6, dpi = 300)
  ggsave(filename = paste0("6. plot_", current_model, "_MAE.png"), plot = p_mae, width = 8, height = 6, dpi = 300)
  ggsave(filename = paste0("6. plot_", current_model, "_R2.png"), plot = p_r2, width = 8, height = 6, dpi = 300)
  ggsave(filename = paste0("6. plot_", current_model, "_MAPE.png"), plot = p_mape, width = 8, height = 6, dpi = 300)
}

#-------------------------------------------------------------------------------
# Table: 1 facet (data augmentation method)
#-------------------------------------------------------------------------------
method_comparison_summary <- performance_summary %>%
  group_by(Balance_Method) %>%
  summarise(
    Mean_RMSE = mean(RMSE_Test, na.rm = TRUE),
    SD_RMSE = sd(RMSE_Test, na.rm = TRUE),
    Mean_MAE = mean(MAE_Test, na.rm = TRUE),
    SD_MAE = sd(MAE_Test, na.rm = TRUE),
    Mean_Rsquared = mean(Rsquared_Test, na.rm = TRUE),
    SD_Rsquared = sd(Rsquared_Test, na.rm = TRUE),
    Mean_MAPE = mean(MAPE_Test, na.rm = TRUE),
    SD_MAPE = sd(MAPE_Test, na.rm = TRUE)
  ) %>%
  arrange(desc(Balance_Method == "NA"), Mean_RMSE)

write.csv(method_comparison_summary, "7. method_comparison_summary.csv")


print(method_comparison_summary)
