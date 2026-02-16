logit = function(x) {
  p = x / 100
  eps = 1e-6
  p = pmax(eps, pmin(1-eps, p))
  logit = log(p / (1 - p))
  return(logit)
}
