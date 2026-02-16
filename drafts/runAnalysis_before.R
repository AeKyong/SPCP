# Install gcc (fortran compiler) in TERMINAL
# brew install gcc
# ~/.R   #dir.create('~/.R') in R
# ~/.R/Makevars  #file.create('~/.R/Makevars') in R
# nano Makevars
# FC = /opt/homebrew/opt/gcc/bin/gfortran
# F77 = /opt/homebrew/opt/gcc/bin/gfortran
# FLIBS = -L/opt/homebrew/opt/gcc/lib
# brew install gcc



rm(list=ls())
devtools::load_all()
# install_github("paobranco/UBL",ref="development")
# install.packages('tinytex')
# tinytex::install_tinytex()

library(reshape)
library(automap) # tar.gz
library(devtools)
library(UBL)
library(e1071)
library(randomForest)
library(caret)
library(miceadds)
library(readxl)


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

# remove all 1 or 0 variable
predictors = predictors[,which(apply(predictors, 2, sum)!= nrow(predictors))] # remove v1, v23
predictors = predictors[,which(apply(predictors, 2, sum)!= 0)]

# combine information  and predictor data
itemDataF = as.data.frame(cbind(itemData[,varNames.info], predictors))

# save final predictor variable names
predictor_vars = names(predictors)


#----------------------------------------
# 0. Descriptive Statistics
#----------------------------------------

hist(itemDataF$pctCTT)
hist(itemDataF$pctIRT)
hist(itemDataF$pctSEM)

summary(itemDataF$pctCTT)
summary(itemDataF$pctIRT)
summary(itemDataF$pctSEM)

#----------------------------------------
# 1. Prepare Data
#----------------------------------------
target_variables = c("pctCTT", "pctIRT", "pctSEM")
all_datasets = list()
target = "pctCTT"
for (target in target_variables) {
  all_datasets[[target]] = prepare_ml_datasets(
    full_data = itemDataF,
    target_variable = target,
    predictor_vars = predictor_vars
  )
}
# hist(all_datasets$pctCTT$train_25g_NA$pct)
# hist(all_datasets$pctCTT$train_random_NA$pct)
#
# hist(all_datasets$pctIRT$train_25g_NA$pct)
# hist(all_datasets$pctIRT$train_random_NA$pct)
#
# hist(all_datasets$pctSEM$train_25g_NA$pct)
# hist(all_datasets$pctSEM$train_random_NA$pct)


#--------------------------------------------
# 2. Data Augmentation
#--------------------------------------------
all_balanced = list()
target_variables = c("pctCTT", "pctIRT")
target = "pctIRT"
for(target in target_variables){
  all_balanced[[target]] = data_augmentation(
    all_datasets = all_datasets,
    target_variable = target)
  }


# Frequency table
freqTables <- c()
target <- "pctCTT" # Example target

all_pcts <- unlist(lapply(c(all_balanced[["pctCTT"]],all_balanced[["pctIRT"]]), function(df) df$pct))
max_val <- ceiling(max(all_pcts, na.rm = TRUE))
for(target in target_variables) {


  # 2. Define the breaks dynamically using this maximum value
  breaks <- c(0, 50, 60, 70, 80, 90, 100, max_val)

  # Use these consistent breaks for all dataframes in the list
  frequency_list <- lapply(all_balanced[[target]], function(df) {
    # The labels=FALSE argument returns integer codes for the bins
    binned_pct <- cut(df$pct, breaks = breaks, include.lowest = TRUE, right = TRUE, labels = FALSE)
    # We use factor to ensure all levels (bins) are present in the table, even if empty
    table(factor(binned_pct, levels = 1:(length(breaks)-1)))
  })

  freq <- as.data.frame(do.call(rbind, frequency_list))
  freq[,"Total"] <- rowSums(freq)
  # freq <- freq[!(rownames(freq) %in% c("test_random", "test_25g")),]
  freq <- freq[c(grep("train_random", rownames(freq)), grep("train_25g", rownames(freq))),]

  last_range_label <- paste0("100-", max_val)
  new_colnames <- c("0-50", "50-60","60-70","70-80","80-90","90-100", last_range_label, "Total")
  colnames(freq) <- new_colnames

  freq$Var <- paste0(target)
  freq <- freq[,c("Var", "0-50", "50-60","60-70","70-80","80-90","90-100", last_range_label, "Total")]
  freqTables <- rbind(freqTables, freq)
}

write.csv(freqTables, "3. Table_Frequency_Balanced Data.csv")

#--------------------------------------------
# 3. Train Data and Evaluate
#--------------------------------------------
models_to_run = c(
  "Ridge"="glmnet",
  "Lasso"="glmnet",
  "SVR"="svmRadial",
  "Random Forest"="rf",
  "XGBoost"="xgbTree",
  "Cubist"="cubist"
)
balance_methods = c("NA", "RO", "GN", "SR", "SG", "IS", "MI", "CT", "NN")

all_performance_results <- list()
all_prediction_dfs <- list()
all_importance_dfs <- list()
all_cv_metrics_dfs <- list()
nsv_results_list <- list()

target = "pctCTT"
for (target in names(all_balanced)) {
  if (is.null(all_balanced[[target]]))  next

  split_type = "random"
  for (split_type in c("random", "25g")) {
    split_name <- ifelse(split_type == "random", "Random Split", "25G Split")

    balance_method = "MI"
    for (balance_method in balance_methods){

      model_name = names(models_to_run)[1]
      for (model_name in names(models_to_run)) {
        model_method <- models_to_run[model_name]

        cat(paste("Training", model_name, "(Standard CV)... "))
        results_standard <- tryCatch({
          train_and_evaluate_model(
            data=all_balanced,
            target_variable=target,
            split = split_type,
            balance_method = balance_method,
            model_method=model_method,
            cv_type = "Standard",
            model_name = model_name
          )
        }, error=function(e) {
          cat(paste("ERROR training", model_name, "(Standard CV):", e$message, "\n"))
          return(NULL)
        })

        if (!is.null(results_standard)) {
          cat("Done.\n")
          results_standard$performance$Target_Variable <- target
          results_standard$performance$Split_Type <- split_name
          results_standard$performance$Balance_Method <- balance_method
          results_standard$performance$Model <- model_name

          if (nrow(results_standard$predictions) > 0) {
            results_standard$predictions$Target_Variable <- target
            results_standard$predictions$Split_Type <- split_name
            results_standard$predictions$Balance_Method <- balance_method
            results_standard$predictions$Model <- model_name
            results_standard$predictions$CV_Type <- "Standard"
          }

          if (nrow(results_standard$importance) > 0) {
            results_standard$importance$Target_Variable <- target
            results_standard$importance$Split_Type <- split_name
            results_standard$importance$Balance_Method <- balance_method
            results_standard$importance$Model <- model_name
            results_standard$importance$CV_Type <- "Standard"
          }

          if (nrow(results_standard$cv_metrics) > 0) {
            results_standard$cv_metrics$Target_Variable <- target
            results_standard$cv_metrics$Split_Type <- split_name
            results_standard$cv_metrics$Balance_Method <- balance_method
            results_standard$cv_metrics$Model <- model_name
            results_standard$cv_metrics$CV_Type <- "Standard"
          }

          all_performance_results[[length(all_performance_results)+1]] <- results_standard$performance
          if (nrow(results_standard$predictions) > 0) all_prediction_dfs[[length(all_prediction_dfs)+1]] <- results_standard$predictions
          if (nrow(results_standard$importance) > 0) all_importance_dfs[[length(all_importance_dfs)+1]] <- results_standard$importance
          if (nrow(results_standard$cv_metrics) > 0) all_cv_metrics_dfs[[length(all_cv_metrics_dfs)+1]] <- results_standard$cv_metrics
        }

        cat(paste("Training", model_name, "(Stratified CV)... "))
        results_stratified <- tryCatch({
          train_and_evaluate_model(
            data=all_balanced,
            target_variable=target,
            split = split_type,
            balance_method = balance_method,
            model_method=model_method,
            cv_type = "Stratified",
            model_name = model_name
          )
        }, error=function(e) {
          cat(paste("ERROR training", model_name, "(Stratified CV):", e$message, "\n"))
          return(NULL)
        })

        if (!is.null(results_stratified)) {
          cat("Done.\n")
          results_stratified$performance$Target_Variable <- target
          results_stratified$performance$Split_Type <- split_name
          results_stratified$performance$Balance_Method <- balance_method
          results_stratified$performance$Model <- model_name

          if (nrow(results_stratified$predictions) > 0) {
            results_stratified$predictions$Target_Variable <- target
            results_stratified$predictions$Split_Type <- split_name
            results_stratified$predictions$Balance_Method <- balance_method
            results_stratified$predictions$Model <- model_name
            results_stratified$predictions$CV_Type <- "Stratified"
          }

          if (nrow(results_stratified$importance) > 0) {
            results_stratified$importance$Target_Variable <- target
            results_stratified$importance$Split_Type <- split_name
            results_stratified$importance$Balance_Method <- balance_method
            results_stratified$importance$Model <- model_name
            results_stratified$importance$CV_Type <- "Stratified"
          }

          if (nrow(results_stratified$cv_metrics) > 0) {
            results_stratified$cv_metrics$Target_Variable <- target
            results_stratified$cv_metrics$Split_Type <- split_name
            results_stratified$cv_metrics$Balance_Method <- balance_method
            results_stratified$cv_metrics$Model <- model_name
            results_stratified$cv_metrics$CV_Type <- "Stratified"
          }

          all_performance_results[[length(all_performance_results)+1]] <- results_stratified$performance
          if (nrow(results_stratified$predictions) > 0) all_prediction_dfs[[length(all_prediction_dfs)+1]] <- results_stratified$predictions
          if (nrow(results_stratified$importance) > 0) all_importance_dfs[[length(all_importance_dfs)+1]] <- results_stratified$importance
          if (nrow(results_stratified$cv_metrics) > 0) all_cv_metrics_dfs[[length(all_cv_metrics_dfs)+1]] <- results_stratified$cv_metrics
        }
      }
    }
  }
}

# conditions = expand.grid(cv = c("Standard", "Stratified"),
#                          model = c("Ridge","Lasso", "SVR", "RandomF", "XGboost", "Cubist"),
#                          balance = c("NA", "RO", "GN", "SR" ,"IS"),
#                          split = c("random", "25g"),
#                          target = c("pctCTT", "pctIRT", "pctSEM")
#                          )


#-----------------------------------------
# 4. Organize result data
#-----------------------------------------
library(dplyr)
model_order <- c("Ridge", "Lasso", "SVR", "Random Forest", "XGBoost", "Cubist")
target_order <- c("pctCTT", "pctIRT", "pctSEM")
target_labels <- c(
  pctCTT = "pctCTT",
  pctIRT = "pctIRT",
  pctSEM = "pctSEM"
)
balance_order <- c("NA","RO","GN","SR", "SG", "IS", "MI", "CT", "NN")

temp_performance_df <- dplyr::bind_rows(all_performance_results)

final_performance_df <- temp_performance_df %>%
  dplyr::mutate(
    Model = factor(Model, levels=model_order, ordered=TRUE),
    Target_Variable = factor(Target_Variable, levels=target_order, ordered=TRUE),
    Balance_Method = factor(Balance_Method, levels = balance_order, ordered=TRUE),
    Rsquared_Overfitting_Drop = Rsquared_Train-Rsquared_Test,
    RMSE_Overfitting_Increase = RMSE_Test-RMSE_Train,
    MAPE_Overfitting_Increase = MAPE_Test-MAPE_Train,
    Rsquared_Relative_Drop = dplyr::case_when(
      abs(Rsquared_Train) > 0.01 ~ (Rsquared_Train-Rsquared_Test)/abs(Rsquared_Train),
      TRUE ~ NA_real_
    ),
    RMSE_Relative_Increase = dplyr::case_when(
      RMSE_Train > 1e-6 ~ (RMSE_Test-RMSE_Train)/RMSE_Train,
      RMSE_Train <= 1e-6 & RMSE_Test <= 1e-6 ~ 0,
      RMSE_Train <= 1e-6 & RMSE_Test > 1e-6 ~ Inf,
      TRUE ~ NA_real_
    ),
    MAPE_Relative_Increase=dplyr::case_when(
      MAPE_Train > 1e-6 ~ (MAPE_Test-MAPE_Train)/MAPE_Train,
      MAPE_Train <= 1e-6 & MAPE_Test <= 1e-6 ~ 0,
      MAPE_Train <= 1e-6 & MAPE_Test > 1e-6 ~ Inf,
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::select(
    Target_Variable, Split_Type, Balance_Method, CV_Type, Model,
    RMSE_Train, MAE_Train, Rsquared_Train, MAPE_Train,
    RMSE_Test, MAE_Test, Rsquared_Test, MAPE_Test,
    Rsquared_Overfitting_Drop, RMSE_Overfitting_Increase, MAPE_Overfitting_Increase,
    Rsquared_Relative_Drop, RMSE_Relative_Increase, MAPE_Relative_Increase
  ) %>%
  dplyr::arrange(Target_Variable, Split_Type,Balance_Method, CV_Type, Model)

final_predictions_df <- dplyr::bind_rows(all_prediction_dfs) %>%
  dplyr::mutate(
    Model=factor(Model, levels=model_order, ordered=TRUE),
    Target_Variable=factor(Target_Variable, levels=target_order, ordered=TRUE)
  )

final_importance_df <- dplyr::bind_rows(all_importance_dfs) %>%
  dplyr::mutate(
    Model=factor(Model, levels=model_order, ordered=TRUE),
    Target_Variable=factor(Target_Variable, levels=target_order, ordered=TRUE)
  )

final_cv_metrics_df <- dplyr::bind_rows(all_cv_metrics_dfs) %>%
  dplyr::mutate(Target_Variable=factor(Target_Variable, levels=target_order, ordered=TRUE))

if (length(nsv_results_list) > 0) {
  final_nsv_removed_df <- dplyr::bind_rows(nsv_results_list) %>%
    dplyr::mutate(Target_Variable=factor(Target_Variable, levels=target_order, ordered=TRUE))
} else {
  final_nsv_removed_df <- data.frame(Target_Variable=character(), Split_Type=character(), Removed_Feature=character(), CV_Type=character())
}

cv_comparison_summary <- final_cv_metrics_df %>%
  dplyr::group_by(Target_Variable, Split_Type, CV_Type, Balance_Method, Model) %>%
  dplyr::summarise(
    RMSE_Mean=mean(RMSE, na.rm=TRUE),
    RMSE_SD=sd(RMSE, na.rm=TRUE),
    MAE_Mean=mean(MAE, na.rm=TRUE),
    MAE_SD=sd(MAE, na.rm=TRUE),
    Rsquared_Mean=mean(Rsquared, na.rm=TRUE),
    Rsquared_SD=sd(Rsquared, na.rm=TRUE),
    MAPE_Mean=mean(MAPE, na.rm=TRUE),
    MAPE_SD=sd(MAPE, na.rm=TRUE),
    .groups="drop"
  ) %>%
  dplyr::mutate(
    Model=factor(Model, levels=model_order, ordered=TRUE),
    Target_Variable=factor(Target_Variable, levels=target_order, ordered=TRUE),
    Balance_Method = factor(Balance_Method, levels= balance_order, ordered=TRUE)
  ) %>%
  dplyr::arrange(Target_Variable, Split_Type, CV_Type, Balance_Method, Model)


write.csv(final_performance_df, "4. performance.csv")
write.csv(final_predictions_df, "4. predictions.csv")
write.csv(final_importance_df, "4. importance.csv")
write.csv(final_cv_metrics_df, "4. cv_metrics.csv")
write.csv(cv_comparison_summary, "4. cv_comparison_summary.csv")



#-------------------------------
# 5. Plot Prediction Results
#-------------------------------
output_dir <- getwd()
target_labels_with_specific_breaks <- c("pctCTT", "pctIRT", "pctSEM")
CV_Types <- c("Standard", "Stratified")
balance_order <- c("NA","RO","GN","SR", "SG", "IS", "MI", "CT")

if (nrow(final_predictions_df) > 0) {
  final_predictions_df <- final_predictions_df %>%
    dplyr::mutate(
      Target_Label = dplyr::case_when(
        Target_Variable == "pctCTT" ~ "pctCTT",
        Target_Variable == "pctIRT" ~ "pctIRT",
        Target_Variable == "pctSEM" ~ "pctSEM",
        TRUE ~ as.character(Target_Variable)
      ),
      Target_Label = factor(Target_Label, levels = target_labels, ordered = TRUE),
      Balance_Method = factor(Balance_Method, levels = balance_order)
    )

  for (target_label_val_factor in target_labels) {
    target_label_val <- as.character(target_label_val_factor)

    plot_data <- final_predictions_df %>% dplyr::filter(Target_Label == target_label_val_factor)

    if (nrow(plot_data) == 0) {
      message(paste("No data for Target_Label:", target_label_val, "- skipping plot."))
      next
    }

    min_val_actual <- min(plot_data$Actual, na.rm = TRUE)
    max_val_actual <- max(plot_data$Actual, na.rm = TRUE)
    min_val_predicted <- min(plot_data$Predicted, na.rm = TRUE)
    max_val_predicted <- max(plot_data$Predicted, na.rm = TRUE)

    overall_min_val <- min(min_val_actual, min_val_predicted, na.rm = TRUE)
    overall_max_val <- max(max_val_actual, max_val_predicted, na.rm = TRUE)

    buffer_min <- ifelse(abs(overall_min_val) < 1, 0.1, 0.05 * abs(overall_min_val))
    buffer_max <- ifelse(abs(overall_max_val) < 1, 0.1, 0.05 * abs(overall_max_val))
    coord_min <- overall_min_val - buffer_min
    coord_max <- overall_max_val + buffer_max

    is_pct_target <- grepl("pct_", target_label_val, ignore.case = TRUE)
    needs_specific_breaks <- target_label_val %in% target_labels_with_specific_breaks

    if (is_pct_target) {
      current_plot_min_bound <- 0
      current_plot_max_bound <- 100

      if (needs_specific_breaks) {
        coord_min <- min(overall_min_val - 5, 40, na.rm = TRUE)
        coord_max <- max(overall_max_val + 5, 100, na.rm = TRUE)
        coord_min <- max(current_plot_min_bound, coord_min, na.rm = TRUE)
        coord_max <- min(current_plot_max_bound, coord_max, na.rm = TRUE)
      } else {
        coord_min <- max(current_plot_min_bound, overall_min_val - 5, na.rm = TRUE)
        coord_max <- min(current_plot_max_bound, overall_max_val + 5, na.rm = TRUE)
      }
    }

    if (any(is.infinite(c(coord_min, coord_max))) || any(is.na(c(coord_min, coord_max))) || coord_min >= coord_max) {
      message(paste("Warning: problematic coord_min/max for", target_label_val,
                    ". Min:", coord_min, "Max:", coord_max, ". Applying failsafe."))
      coord_min <- floor(overall_min_val / 10) * 10
      coord_max <- ceiling(overall_max_val / 10) * 10
      if (coord_min >= coord_max) {
        coord_min <- if (is.finite(overall_min_val)) overall_min_val - 1 else 0
        coord_max <- if (is.finite(overall_max_val)) overall_max_val + 1 else 100
      }
      message(paste("   New Min:", coord_min, "New Max:", coord_max))
    }

    for (CV_Type in CV_Types) {
      plot_data_cv <- plot_data %>% dplyr::filter(CV_Type == !!CV_Type)

      if (nrow(plot_data_cv) == 0) {
        message(paste("No data for Target_Label:", target_label_val, "and CV_Type:", CV_Type, "- skipping plot."))
        next
      }

      prediction_plot <- ggplot(plot_data_cv, aes(x = Actual, y = Predicted)) +
        geom_point(alpha = 0.5, color = "blue") +
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
        facet_grid(Split_Type + Balance_Method ~ Model) +
        labs(x = "Actual Values", y = paste("Predicted Values (CV:", CV_Type, ")")) +
        theme_bw() +
        theme(
          strip.text = element_text(size = 8, face = "bold"),
          axis.text = element_text(size = 7)
        )

      if (needs_specific_breaks) {
        final_breaks <- c(40, 60, 80, 100)
        final_breaks <- final_breaks[final_breaks >= coord_min & final_breaks <= coord_max]

        prediction_plot <- prediction_plot +
          scale_x_continuous(breaks = final_breaks, limits = c(coord_min, coord_max)) +
          scale_y_continuous(breaks = final_breaks, limits = c(coord_min, coord_max))
      } else if (is_pct_target) {
        breaks_pct <- scales::pretty_breaks(n = 5)(c(coord_min, coord_max))
        if (coord_min <= 0 && !0 %in% breaks_pct) breaks_pct <- sort(unique(c(0, breaks_pct)))
        if (coord_max >= 100 && !100 %in% breaks_pct) breaks_pct <- sort(unique(c(breaks_pct, 100)))

        prediction_plot <- prediction_plot +
          scale_x_continuous(breaks = breaks_pct, limits = c(coord_min, coord_max)) +
          scale_y_continuous(breaks = breaks_pct, limits = c(coord_min, coord_max))
      } else {
        prediction_plot <- prediction_plot +
          scale_x_continuous(limits = c(coord_min, coord_max)) +
          scale_y_continuous(limits = c(coord_min, coord_max))
      }

      file_name_label <- tolower(target_label_val)
      file_name_label <- gsub("[^a-z0-9_]+", "_", file_name_label)
      file_name_label <- gsub("_+", "_", file_name_label)
      file_name_label <- sub("^_|_$", "", file_name_label)

      plot_file <- file.path(output_dir, paste0("5. predictions_plot_", file_name_label, "_", CV_Type, ".png"))
      ggsave(plot_file, plot = prediction_plot, width = 16, height = 10, dpi = 300)

      # emf_file <- file.path(output_dir, paste0("5. predictions_plot_", file_name_label, "_", CV_Type, ".emf"))
      # tryCatch({
      #   devEMF::emf(file = emf_file, width = 16, height = 10)
      #   print(prediction_plot)
      #   dev.off()
      # }, error = function(e) {
      #   message(paste("Failed to save EMF for", file_name_label, "_", CV_Type, ":", e$message))
      #   if (grDevices::dev.cur() != 1) dev.off()
      # })
    }
  }
  message("\nPrediction comparison plots saved.")
} else {
  message("\nNo prediction data available to generate plots.")
}



