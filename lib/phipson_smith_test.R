## Exact p-value for Monte-Carlo tests as per Phipson & Smith 2010,
## section 4.
## Phipson, B., & Smyth, G. K. (2010). Permutation p-values should
## never be zero: calculating exact p-values when permutations are
## randomly drawn. Statistical Applications in Genetics and Molecular
## Biology, 9(1). https://doi.org/10.2202/1544-6115.1585
phipson_smith_test <- function(observed, mc_simulations) {
  n_mc <- length(mc_simulations)
  (sum(abs(mc_simulations) >= abs(observed)) + 1)/(n_mc + 1)
}
