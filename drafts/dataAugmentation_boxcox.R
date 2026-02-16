library(tidyverse)
library(caret)
library(MASS) # For Box-Cox transformation
library(randomForest)
library(gbm)
library(Metrics) # For evaluation metrics
library(e1071) # For skewness calculation
install.packages("UBL")

# --- 1. Load Your Actual Data ---
itemData = readxl::read_excel("data/math_g3_feature.xlsx", skip =1, col_names =TRUE, sheet = "Coding")
your_data = itemData

# --- Verify Your Data and Column Names ---
dependent_var_name <- "정답률(변환후)"

# --- 2. Exploratory Data Analysis (EDA) and Skewness Check ---
hist(your_data[[dependent_var_name]], main = paste("Histogram of", dependent_var_name), xlab = dependent_var_name)


# # --- 3. Data Preprocessing ---
# all_col_names <- colnames(your_data)
#
# # Find the index of your dependent variable(s) to exclude them from features
# dep_var_indices <- which(all_col_names %in% c(dependent_var_name, dependent_var_sem_name))
#
#
# start_feature_col_index <- 13
#
# # Get candidate feature column names starting from the specified index
candidate_feature_col_names <- paste0("x", 1:ncol(your_data))
colnames(your_data) = candidate_feature_col_names

# Filter out the dependent variables from the candidate features
dependent_var_name = "x9"
dependent_var_sem_name = "x12"
feature_col_names = candidate_feature_col_names[-c(1:12)]


# Convert categorical features (including binary if loaded as char/logical) to factors
for (col_name in feature_col_names) {
  if (is.character(your_data[[col_name]]) || is.logical(your_data[[col_name]])) {
    your_data[[col_name]] <- as.factor(your_data[[col_name]])
  }
}


# --- 4. Addressing Skewness in Dependent Variable (Regression) ---
current_dependent_var <- your_data[[dependent_var_name]]

# Check for non-positive values before Box-Cox. Add an offset if necessary.
offset_val <- 0
min_dep_var <- min(current_dependent_var)
if (min_dep_var <= 0) {
  cat("Warning: Dependent variable '", dependent_var_name, "' contains non-positive values. Adding a small constant for Box-Cox.\n", sep="")
  offset_val <- abs(min_dep_var) + 0.001 # A small positive constant
  your_data$dep_var_bc_input <- current_dependent_var + offset_val
} else {
  your_data$dep_var_bc_input <- current_dependent_var
}

boxcox_model_dep_var <- boxcox(your_data$dep_var_bc_input ~ 1)
lambda_dep_var <- boxcox_model_dep_var$x[which.max(boxcox_model_dep_var$y)]
your_data$dependent_var_transformed <- (your_data$dep_var_bc_input^lambda_dep_var - 1) / lambda_dep_var

hist(your_data$dependent_var_transformed, main = paste("Histogram of", dependent_var_name, "(Box-Cox)"), xlab = "Transformed Dependent Variable")


# --- 5. Data Splitting (Training and Testing Sets) ---
set.seed(42)
# Create partition based on the transformed dependent variable
train_index <- createDataPartition(your_data$dependent_var_transformed, p = 0.7, list = FALSE)
train_data <- your_data[train_index, ]
test_data <- your_data[-train_index, ]


# --- 6. Machine Learning Models for Regression ---
train_control <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = TRUE
)

# Create the model formula using the transformed dependent variable and selected features
model_formula <- as.formula(paste("dependent_var_transformed ~", paste(feature_col_names, collapse = " + ")))



# --- Model 2: Random Forest (RF) ---
set.seed(123)
model_rf <- train(
  model_formula,
  data = train_data,
  method = "rf",
  trControl = train_control,
  importance = TRUE
)
print(model_rf)
plot(varImp(model_rf))

predictions_rf <- predict(model_rf, newdata = test_data)
rmse_rf <- rmse(test_data$dependent_var_transformed, predictions_rf)
mae_rf <- mae(test_data$dependent_var_transformed, predictions_rf)
r2_rf <- cor(test_data$dependent_var_transformed, predictions_rf)^2



# --- Model 3: GBM ---
set.seed(123)
model_gbm <- train(
  model_formula,
  data = train_data,
  method = "gbm",
  trControl = train_control
)
print(model_gbm)
predictions_gbm <- predict(model_gbm, newdata = test_data)
rmse_gbm <- rmse(test_data$dependent_var_transformed, as.numeric(predictions_gbm))
mae_gbm <- mae(test_data$dependent_var_transformed, predictions_gbm)
r2_gbm <- cor(test_data$dependent_var_transformed, predictions_gbm)^2
r2_gbm

# --- Model 4: lasso ---
set.seed(123)
model_lasso <- train(
  model_formula,
  data = train_data,
  method = "glmnet",
  trControl = train_control
)
print(model_lasso)
predictions_lasso <- predict(model_lasso, newdata = test_data)
rmse_lasso <- rmse(test_data$dependent_var_transformed, as.numeric(predictions_lasso))
mae_lasso <- mae(test_data$dependent_var_transformed, predictions_lasso)
r2_lasso <- cor(test_data$dependent_var_transformed, predictions_lasso)^2
r2_lasso

# --- 7. Inverse Transform Predictions ---
inverse_boxcox <- function(y_transformed, lambda, offset) {
  if (lambda == 0) {
    return(exp(y_transformed) - offset)
  } else {
    return((y_transformed * lambda + 1)^(1/lambda) - offset)
  }
}

# Get the original dependent variable values from the test set for comparison
original_test_dep_var <- test_data[[dependent_var_name]]


# Apply inverse transform to RF predictions
predictions_rf_original_scale <- inverse_boxcox(predictions_rf, lambda_dep_var, offset = offset_val)
rmse_rf_original_scale <- rmse(original_test_dep_var, predictions_rf_original_scale)
mae_rf_original_scale <- mae(original_test_dep_var, predictions_rf_original_scale)
r2_rf

# Apply inverse transform to GBM predictions
predictions_gbm_original_scale <- inverse_boxcox(predictions_gbm, lambda_dep_var, offset = offset_val)
rmse_gbm_original_scale <- rmse(original_test_dep_var, predictions_gbm_original_scale)
mae_gbm_original_scale <- mae(original_test_dep_var, predictions_gbm_original_scale)
r2_gbm

# Apply inverse transform to lasso predictions
predictions_lasso_original_scale <- inverse_boxcox(predictions_lasso, lambda_dep_var, offset = offset_val)
rmse_lasso_original_scale <- rmse(original_test_dep_var, predictions_lasso_original_scale)
mae_lasso_original_scale <- mae(original_test_dep_var, predictions_lasso_original_scale)
r2_lasso


# --- 8. Compare Models ---
plot(original_test_dep_var, predictions_rf_original_scale,
     main = paste("RF: Actual vs. Predicted (Original Scale for", dependent_var_name, ")"),
     xlab = paste("Actual", dependent_var_name), ylab = paste("Predicted", dependent_var_name))

abline(a = 0, b = 1, col = "red")


plot(original_test_dep_var, predictions_gbm_original_scale,
     main = paste("RF: Actual vs. Predicted (Original Scale for", dependent_var_name, ")"),
     xlab = paste("Actual", dependent_var_name), ylab = paste("Predicted", dependent_var_name))

abline(a = 0, b = 1, col = "red")


plot(original_test_dep_var, predictions_lasso_original_scale,
     main = paste("RF: Actual vs. Predicted (Original Scale for", dependent_var_name, ")"),
     xlab = paste("Actual", dependent_var_name), ylab = paste("Predicted", dependent_var_name))

abline(a = 0, b = 1, col = "red")



# Create a data frame for comparison
comparison_table_rf <- data.frame(
  Actual_Value = original_test_dep_var,
  Predicted_Value = predictions_rf_original_scale
)

# You can sort it by Actual_Value for easier review if desired
comparison_table_rf <- comparison_table_rf %>%
  arrange(Actual_Value)

# Print the first few rows of the table
cat("\n--- Random Forest: Actual vs. Predicted Values (Original Scale) ---\n")
print(head(comparison_table_rf)) # Print the first 6 rows


# You can also add residuals to the table for more detailed analysis
comparison_table_rf_with_residuals <- comparison_table_rf %>%
  mutate(Residual = Actual_Value - Predicted_Value)

comparison_table_rf_with_residuals
