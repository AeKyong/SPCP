irt2PLprobability = function(a, b, theta){
# browser()
  nItems = length(a)
  pTheta = NULL
  for (item in 1:nItems){
    logit = a[item]*(theta - b[item])
    p = exp(logit)/(1+exp(logit))

    pTheta = cbind(pTheta, p)
    colnames(pTheta)[item] = paste0("item", item)
  }


  return(pTheta)
}
