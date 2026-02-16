create_standard_folds <- function(y, k=10, seed=123) {
  set.seed(seed)
  n <- length(y)
  if (n < k) {
    warning("Number of observations is less than number of folds. Reducing k to", n)
    k <- n
  }
  folds <- createFolds(y, k=k, list=TRUE, returnTrain=TRUE)
  return(folds)
}
