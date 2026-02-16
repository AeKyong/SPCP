assmpCheck = function(variableName, data){

  # get colnames
  colNames = names(data)

  var = colNames[grep(variableName, colNames)]

  # Concatenate the predictors
  predictors = paste(var, collapse = "+")

  # Create the formula (assuming 'y' is the response variable)
  formula = as.formula(paste("probIRT ~", predictors))

  # Run the linear model (replace 'data' with your actual data frame)
  model = lm(formula, data = data)

  pdf(paste0("lm_plot_part3_",variableName,".pdf"))
  par(mfrow=c(2,2))
  plot(model)
  dev.off()
}
