## compute slopes for modified climate data: remove El Niño effects by
## regressing MEI against local SAT and use residuals

request("mei_gistemp")
request("mei_hadcrut")
request("mei_noaa")

mei_cor <- list(gistemp = mei_gistemp,
               hadcrut = mei_hadcrut,
               noaa = mei_noaa)

## wrapper function wrapping the slope test functions after
## removing El Niño influence; the function caches the results,
## following the "slopes_meiresid_[dataset]_[period]" scheme.
period_slopes2 <- function(.name, .periods, .n = N_MC, sig_only = SIG_ENSO_ONLY) {


  if (SIG_ENSO_ONLY) {
    temp <- mei_cor[[.name]] %>% 
      unnest(cmei) %>% 
      mutate(data = ifelse(p < P_MEI,
                           purrr::map(data, remove_enso),
                           purrr::map(data, nothing)))
  } else {
    temp <- mei_cor[[.name]] %>% 
      unnest(cmei) %>%
      mutate(data = ifelse(!is.na(p),
                           purrr::map(data, remove_enso),
                           purrr::map(data, nothing)))
  }
  
  .slope_fun <- function(x) {
    if (!PARALLEL) {
      set_nobs(nrow(temp))
    }
    res <- temp %>% 
      mutate(sdif = purrr::map(data, slope_diff, n = .n,
                               wp = x$wp,
                               hp = x$hp,
                               parallel = PARALLEL)
             , chow = purrr::map_dbl(data, chow_test,
                                     wp = x$wp,
                                     hp = x$hp)
      )  
    if (!PARALLEL) {
      reset_counter()
    }
    res
  }
  
  if (PARALLEL) {
    .slopes <- mclapply(.periods, .slope_fun, mc.cores = NCORES)
  } else {
    .slopes <- lapply(.periods, .slope_fun)
  }
  
  outnames <- paste("slopes_meiresid", .name, names(.slopes), sep = "_")
  
  mapply(freeze, outnames, .slopes)
  
}

period_slopes2("gistemp", PERIODS)
period_slopes2("hadcrut", PERIODS)
period_slopes2("noaa", PERIODS)
