augCAT = function(dat, target, predictors, nSyn, base_swap_rate = 0.1, threshold){

  x_mat = as.matrix(dat[, predictors])
  y = dat[[target]]

  # logit and scale y
  y_logit = logit(y)
  y_logit_mean = mean(y_logit, na.rm=TRUE)
  y_logit_sd = sd(y_logit, na.rm=TRUE)
  y_scaled = (y_logit-y_logit_mean)/y_logit_sd

  # fit Ridge regression
  fit_cv = cv.glmnet(x_mat, y_scaled, alpha = 0.5)

  # get swap_rate from  coefficients not to generate exactly the same x value
  coeffs = as.matrix(coef(fit_cv, s = "lambda.min"))[-1, 1]
  importance = abs(coeffs)
  norm_importance = (importance - min(importance))/ (max(importance) - min(importance) + .Machine$double.xmin)
  var_swap_rates = base_swap_rate * (1-norm_importance)
  var_swap_rates = pmax(var_swap_rates, 0.01)

  # find X profiles' candidates in which the y value is lower than the threshold
  x_can = dat[which(dat$pct < threshold), which(!names(dat) %in% "pct")]

  # generate synthetic x matrix
  idx = sample(1:nrow(x_can), nSyn, replace = TRUE)
  xSyn = x_can[idx, predictors]
  for(var in predictors){
    current_rate = var_swap_rates[var]
    swap_idx = which(runif(nSyn) < current_rate)
    if(length(swap_idx) > 0){
      xSyn[swap_idx, var] = sample(x_can[, var], length(swap_idx), replace = TRUE)
    }
  }

  # predict y
  xSyn = as.matrix(xSyn)
  y_pred = as.numeric(predict(fit_cv, newx = xSyn, s = "lambda.min"))
  y_scaled_fitted = as.numeric(predict(fit_cv, newx = x_mat, s = "lambda.min"))

  # make residual pool by quantile of y_scaled
  quantiles = quantile(y_scaled, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm=T)
  resid_all = (y_scaled - y_scaled_fitted)
  i=1
  resid_pools = list()
    for(i in 1:(length(quantiles)-1)){
      idx = which(y_scaled >= quantiles[i] & y_scaled < quantiles[i+1])
      resid_pools[[i]] = resid_all[idx]
    }

  # add residual to predicted y
  ySyn_logit = rep(NA, length(y_pred))
  for(j in 1:length(y_pred)){
    target_val = y_pred[j]

    if (target_val < quantiles[2]){p_idx = 1
    } else if(target_val < quantiles[3]){p_idx = 2
    } else if(target_val < quantiles[4]){p_idx = 3
    } else if(target_val < quantiles[5]){p_idx = 4
    } else {p_idx = 5
    }

    ySyn_logit[j] = target_val + sample(resid_pools[[p_idx]], 1, replace = TRUE)
  }

  # rescale and inv_logit y
  ySyn_rescale = (y_logit_sd*ySyn_logit) + y_logit_mean
  ySyn = inv_logit(ySyn_rescale)

  # combine x and y
  syn_dat = cbind(ySyn, xSyn)
  colnames(syn_dat)[1] = target
  df = rbind(dat, syn_dat)

  return(df)
}
