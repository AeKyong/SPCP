irt2PLinformation = function(theta, a, b) {
# browser()
  prob = irt2PLprobability(theta = theta, a = a, b = b)

  info = matrix(NA, nrow = length(theta), ncol = length(a))
  for(item in 1:length(a)){
    info[,item] = a[item]^2 * prob[,item] * (1-prob[,item])
  }


  return(info)
}
