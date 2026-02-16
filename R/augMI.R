augMI = function(dat, fix_vars, ord_vars, syn_vars){

  orig_names = colnames(dat)
  cols_to_remove = names(dat)[sapply(dat, function(col) length(unique(col)) == 1)]

  dat_reduced = dat
  constant_info = list()
  if(length(cols_to_remove) > 0 ){
    constant_info = lapply(dat[cols_to_remove], '[[', 1)
    dat_reduced = dat[, !(names(dat) %in% cols_to_remove), drop=FALSE]
    fix_vars = setdiff(fix_vars, cols_to_remove)
    ord_vars = setdiff(ord_vars, cols_to_remove)
    syn_vars = setdiff(syn_vars, cols_to_remove)
  }

  synData = miceadds::syn_da(dat=dat_reduced, syn_vars=syn_vars, fix_vars=fix_vars,
                             ord_vars=ord_vars, da_noise=0.5, imp_maxit=2, use_pls=TRUE, ncomp=20,
                             exact_regression=TRUE, exact_marginal=TRUE)$dat_syn

  combinedData = rbind(dat_reduced, synData)

  if (length(cols_to_remove) > 0) {
    for (col in cols_to_remove) {
      combinedData[[col]] = constant_info[[col]]
    }
  }

  return(combinedData[,orig_names, drop = FALSE])
}
