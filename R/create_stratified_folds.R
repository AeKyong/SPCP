create_stratified_folds <- function(y, k=10, seed=123) {
  set.seed(seed)
  n <- length(y)
  if (n < k) {
    warning("Number of observations is less than number of folds. Falling back to standard k-fold.")
    return(createFolds(y, k=min(k,n), list=TRUE, returnTrain=TRUE))
  }

  n_bins <- min(10, length(unique(y)))
  if (n_bins < 2) {
    warning("Too few unique values in target variable for stratification. Falling back to standard k-fold.")
    return(createFolds(y, k=k, list=TRUE, returnTrain=TRUE))
  }

  breaks <- quantile(y, probs=seq(0,1,length.out=n_bins+1), na.rm=TRUE)
  bins <- cut(y, breaks=breaks, include.lowest=TRUE, labels=FALSE)

  bin_counts <- table(bins)
  if (any(bin_counts < k)) {
    warning("Some bins have fewer samples than folds. Falling back to standard k-fold.")
    return(createFolds(y, k=k, list=TRUE, returnTrain=TRUE))
  }

  folds <- createFolds(bins, k=k, list=TRUE, returnTrain=TRUE)
  return(folds)
}
