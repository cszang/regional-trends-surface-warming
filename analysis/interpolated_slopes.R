## compute slopes for modified climate data: interpolate el-nino years
## 97/98 for grid cells with significant correlation with MEI

request("mei_gistemp")
request("mei_hadcrut")
request("mei_noaa")

mei_cor <- list(gistemp = mei_gistemp,
               hadcrut = mei_hadcrut,
               noaa = mei_noaa)

## wrapper function wrapping the slope test functions after
## interpolating the 97/98 El Nino; the function caches the results,
## following the "slopes_interp9798_[dataset]_[period]" scheme.
period_slopes2 <- function(.name, .periods, .n = 1000) {
  
  temp <- mei_cor[[.name]] %>% 
    unnest(cmei) %>% 
    mutate(data = ifelse(p < P_MEI,
                         purrr::map(data, interp9798),
                         purrr::map(data, nothing)))
  
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
  
  outnames <- paste("slopes_interp9798", .name, names(.slopes), sep = "_")
  
  mapply(freeze, outnames, .slopes)
  
}

period_slopes2("gistemp", PERIODS)
period_slopes2("hadcrut", PERIODS)
period_slopes2("noaa", PERIODS)
