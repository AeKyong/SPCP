mape <- function(actual, predicted) {
  if (length(actual) != length(predicted)) stop("Actual and predicted vectors must have the same length")
  epsilon <- 1e-10
  mean(abs((actual - predicted) / pmax(abs(actual), epsilon))) * 100
}
