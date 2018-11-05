## correlate IPO with all gridcells, similar to ENSO, but: detrend
## temps before correlation! and: 2x 9 yr moving avg

## TODO check the Dai 2015 approach using CMIP5 ensemble GHG trend

if (!request("ipo_gistemp")) {

  double_smooth <- function(df, n = 9) {
    sm <- rollmean(df$temp, k = n, align = "right")
    sm <- rollmean(sm, k = n, align = "right")
    tibble(
      year = df$year[-c(1:(2 * n - 2))],
      temp = sm
    )
  }

  correlate_ipo <- function(x) {
    x <- double_smooth(detrend_df(x))
    xm <- left_join(x, ipo_9, by = "year") %>% na.omit()
    ct <- cor.test(xm$temp, xm$ipo)
    tibble(cor = ct$estimate, p = ct$p.value)
  }

  ipo_gistemp %<f-% {
    temp_gistemp %>%
      mutate(cipo = purrr::map(data, correlate_ipo)) 
  }
  
  ipo_gistemp_fdr %<f-% {
    ipo_gistemp$cipo %>% 
      get_pval_from_tibble %>% 
      fdr_fn %>% 
      put_pval_in_tibble(ipo_gistemp, ., where = 'cipo')
    }
  
  ipo_noaa %<f-% {
    temp_noaa %>%
      mutate(cipo = purrr::map(data, correlate_ipo))
  }
  
  ipo_noaa_fdr %<f-% {
    ipo_noaa$cipo %>%
      get_pval_from_tibble %>%
      fdr_fn %>%
      put_pval_in_tibble(ipo_noaa, ., where = 'cipo')
  }
  
  ipo_hadcrut %<f-% {
    temp_hadcrut %>%
      mutate(cipo = purrr::map(data, correlate_ipo))
  }
  
  ipo_hadcrut_fdr %<f-% {
    ipo_hadcrut$cipo %>%
      get_pval_from_tibble %>%
      fdr_fn %>%
      put_pval_in_tibble(ipo_hadcrut, ., where = 'cipo')
  }
}

