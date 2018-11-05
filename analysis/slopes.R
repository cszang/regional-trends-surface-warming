## compute slopes for vanilla climate data

temps <- list(gistemp = temp_gistemp,
              hadcrut = temp_hadcrut,
              noaa = temp_noaa)

## wrapper function wrapping the slope test functions; the function
## caches the results, following the "slopes_[dataset]_[period]"
## scheme.
period_slopes <- function(.name, .periods, .n = N_MC) {
  
  temp <- temps[[.name]]
  
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
  
  outnames <- paste("slopes", .name, names(.slopes), sep = "_")
  
  mapply(freeze, outnames, .slopes)
  
}

period_slopes("gistemp", PERIODS)
period_slopes("hadcrut", PERIODS)
period_slopes("noaa", PERIODS)
