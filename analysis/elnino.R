## compute correlation of sst with mei index
## pvales adjusted for multiple comparisons using fdr method

mei <- read.table("data/mei.txt", header = TRUE)
mei <- mei %>% gather(month, mei, -YEAR) %>%
  dplyr::rename(year = YEAR) %>% 
  group_by(year) %>%
  dplyr::summarise(mei = mean(mei))
  
correlate_mei <- function(x) {
  x <- detrend_df(x)
  xm <- left_join(x, mei, by = "year") %>% na.omit()
  ct <- cor.test(xm$temp, xm$mei)
  tibble(cor = ct$estimate, p = ct$p.value)
}

mei_gistemp_raw %<f-% {
  temp_gistemp %>%
    mutate(cmei = purrr::map(data, correlate_mei))
}

mei_gistemp %<f-% {
  mei_gistemp_raw$cmei %>%
    get_pval_from_tibble %>%
    fdr_fn %>%
    put_pval_in_tibble(mei_gistemp_raw, ., where = 'cmei')
}

mei_noaa_raw %<f-% {
  temp_noaa %>%
    mutate(cmei = purrr::map(data, correlate_mei))
}

mei_noaa %<f-% {
  mei_noaa_raw$cmei %>%
    get_pval_from_tibble %>%
    fdr_fn %>%
    put_pval_in_tibble(mei_noaa_raw, ., where = 'cmei')
}

mei_hadcrut_raw %<f-% {
  temp_hadcrut %>%
    mutate(cmei = purrr::map(data, correlate_mei))
}

mei_hadcrut %<f-% {
  mei_hadcrut_raw$cmei %>%
    get_pval_from_tibble %>%
    fdr_fn %>%
    put_pval_in_tibble(mei_hadcrut_raw, ., where = 'cmei')
}
