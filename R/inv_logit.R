inv_logit <- function(logit) {
  max_exponent = log(.Machine$double.xmax) - 10
  logit_capped = pmax(-max_exponent, pmin(max_exponent, logit))
  p = 1 / (1 + exp(-logit_capped))
  return(p * 100)
}
