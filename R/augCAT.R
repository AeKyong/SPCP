augCAT = function(dat, target, predictors, nSyn, base_swap_rate = 0.1){
  x_mat = as.matrix(dat[, predictors])
  y_vec = dat[[target]]

  # logit and scale y
  y_vec_logit = logit(y_vec)
  mean_y = mean(y_vec_logit, na.rm=TRUE)
  sd_y = sd(y_vec_logit, na.rm=TRUE)
  y_vec_scaled = (y_vec_logit-mean_y)/sd_y

  # fit Ridge regression
  fit_cv = cv.glmnet(x_mat, y_vec_scaled, alpha = 0.5)

  # coeff and swap_rate
  coeffs = as.matrix(coef(fit_cv, s = "lambda.min"))[-1, 1]
  importance = abs(coeffs)
  norm_importance = (importance - min(importance))/ (max(importance) - min(importance) + .Machine$double.xmin)
  var_swap_rates = base_swap_rate * (1-norm_importance)
  var_swap_rates = pmax(var_swap_rates, 0.01)

  # generate synthetic x matrix
  idx = sample(1:nrow(dat), nSyn, replace = TRUE)
  xSyn = dat[idx, predictors]
  for(var in predictors){
    current_rate = var_swap_rates[var]
    swap_idx = which(runif(nSyn) < current_rate)
    if(length(swap_idx) > 0){
      xSyn[swap_idx, var] = sample(dat[, var], length(swap_idx), replace = TRUE)
    }
  }

  # predict y
  xSyn_mat = as.matrix(xSyn)
  yPred = as.numeric(predict(fit_cv, newx = xSyn_mat, s = "lambda.min"))
  fitted_y = as.numeric(predict(fit_cv, newx = x_mat, s = "lambda.min"))

  # residual pool
  quants = quantile(y_vec_scaled, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm=T)
  resid_all = (y_vec_scaled - fitted_y)
  i=1
  resid_pools = list()
    for(i in 1:(length(quants)-1)){
      idx = which(y_vec_scaled >= quants[i] & y_vec_scaled < quants[i+1])
      resid_pools[[i]] = resid_all[idx]
    }

# browser()
  ySyn_logit = rep(NA, length(yPred))
  for(j in 1:length(yPred)){
    target_val = yPred[j]

    if (target_val < quants[2]){p_idx = 1
    } else if(target_val < quants[3]){p_idx = 2
    } else if(target_val < quants[4]){p_idx = 3
    } else if(target_val < quants[5]){p_idx = 4
    } else {p_idx = 5
    }

    ySyn_logit[j] = target_val + sample(resid_pools[[p_idx]], 1, replace = TRUE)
  }

  # rescale and inv_logit y
  ySyn_rescale = (sd_y*ySyn_logit) + mean_y
  ySyn = inv_logit(ySyn_rescale)

  # combine x and y
  syn.dat = cbind(ySyn, xSyn)
  colnames(syn.dat)[1] = target
  df = rbind(dat[, c(target, predictors)], syn.dat)

  return(df)
}
