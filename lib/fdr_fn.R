# function for correcting the p-values
# applies fdr method
# returns p-values that are corrected for multiple testing
# corrected p-values replace existing p-values and keep same name...

get_pval_from_tibble<- function(data){
 as.numeric(as.character(unlist(lapply(data, function(x) {x$p}))))
}

fdr_fn<- function(pval){
  n<- length(pval)
  ind<- order(pval, decreasing = FALSE)
  ind2<- order(ind) # necessary
  ord_pval<- pval %>% sort 
  ord_pval<- ord_pval*n/(1:n)
  # key step! explanation in Yekutieli and Benjamini (1999) - equation 3
  for(i in (n-1):1) ord_pval[i]<- min(ord_pval[i+1], ord_pval[i])
  ord_pval<- ord_pval[ind2] 
  return(ord_pval)
}

put_pval_in_tibble<- function(data, pval, where = NULL){
  n<- nrow(data)
  for(i in 1:n) data[where][[1]][[i]]$p<- pval[i]
  return(data)
}
