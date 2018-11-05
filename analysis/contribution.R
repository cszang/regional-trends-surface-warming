## contribution of each single grid cell to the global temperature
## trend

request_all("ipo_")
request_all("mei_")
request_all("slopes_")

### local defuns ----------------------------------------------------

period_slope <- function(x, period) {
  x <- x %>% filter(year %in% period)
  ## to scale or not to scale, that is the question!
  ## x$temp <- scale(x$temp)
  if (!all(x$temp == 0)) {
    lm(temp ~ year - 1, data = x) %>% coef
  } else {
    0
  }
}

wp_1 <- 1972:2000
wp_2 <- 1972:1999
wp_3 <- 1972:1998
wp_4 <- 1972:1997
hp_1 <- 2001:2014
hp_2 <- 2000:2014
hp_3 <- 1999:2013
hp_4 <- 1998:2012

mod_test_of_indep<- function(chisq, cor_factor){
 
  return(paste('correction factor: ', round(cor_factor,4), ', adjusted X square: ', round(unname(chisq$statistic/(1+cor_factor)),4), 
                 ' p-value = ', round(unname(pchisq(chisq$statistic/(1+cor_factor),df = 1, lower.tail = FALSE)),10)))
      
}

mod_test_wrapper<- function(data, var1, var2, W){
  data<- data %>% select(lat, lon, var1, var2) 
  data[var1]<- data[var1]*1
  data[var2]<- data[var2]*1
  cor_factor<- correction_factor(data,var1,var2)
  mod_test_of_indep(W, cor_factor)
}

### compute trends for all WP/HP combinations -----------------------

attr_slopes_gistemp %<f-% { temp_gistemp %>%
  mutate(
    slope_wp_1 = purrr::map_dbl(data, period_slope, wp_1),
    slope_wp_2 = purrr::map_dbl(data, period_slope, wp_2),
    slope_wp_3 = purrr::map_dbl(data, period_slope, wp_3),
    slope_wp_4 = purrr::map_dbl(data, period_slope, wp_4),
    slope_hp_1 = purrr::map_dbl(data, period_slope, hp_1),
    slope_hp_2 = purrr::map_dbl(data, period_slope, hp_2),
    slope_hp_3 = purrr::map_dbl(data, period_slope, hp_3),
    slope_hp_4 = purrr::map_dbl(data, period_slope, hp_4)
  )
}

attr_slopes_noaa %<f-% { temp_noaa %>%
  mutate(
    slope_wp_1 = purrr::map_dbl(data, period_slope, wp_1),
    slope_wp_2 = purrr::map_dbl(data, period_slope, wp_2),
    slope_wp_3 = purrr::map_dbl(data, period_slope, wp_3),
    slope_wp_4 = purrr::map_dbl(data, period_slope, wp_4),
    slope_hp_1 = purrr::map_dbl(data, period_slope, hp_1),
    slope_hp_2 = purrr::map_dbl(data, period_slope, hp_2),
    slope_hp_3 = purrr::map_dbl(data, period_slope, hp_3),
    slope_hp_4 = purrr::map_dbl(data, period_slope, hp_4)
  )
}

attr_slopes_hadcrut %<f-% { temp_hadcrut %>%
  mutate(
    slope_wp_1 = purrr::map_dbl(data, period_slope, wp_1),
    slope_wp_2 = purrr::map_dbl(data, period_slope, wp_2),
    slope_wp_3 = purrr::map_dbl(data, period_slope, wp_3),
    slope_wp_4 = purrr::map_dbl(data, period_slope, wp_4),
    slope_hp_1 = purrr::map_dbl(data, period_slope, hp_1),
    slope_hp_2 = purrr::map_dbl(data, period_slope, hp_2),
    slope_hp_3 = purrr::map_dbl(data, period_slope, hp_3),
    slope_hp_4 = purrr::map_dbl(data, period_slope, hp_4)
  )
}

### Prepare data to have all necessary information in one tibble ----

## - significant trend changes
## - IPO-correlation
## - trends for different WP/HP setups

clean_slopes_gistemp_R <- slopes_gistemp_R_fdr %>%
  select(-data) %>%
  unnest(sdif) %>%
  rename(chow_1 = chow,
         delta_1 = slope_delta,
         sig_1 = delta_sig)

clean_slopes_gistemp_p2 <- slopes_gistemp_p2_fdr %>%
  select(-data) %>%
  unnest(sdif) %>%
  rename(chow_2 = chow,
         delta_2 = slope_delta,
         sig_2 = delta_sig)

clean_slopes_gistemp_p3 <- slopes_gistemp_p3_fdr %>%
  select(-data) %>%
  unnest(sdif) %>%
  rename(chow_3 = chow,
         delta_3 = slope_delta,
         sig_3 = delta_sig)

clean_slopes_gistemp_p4 <- slopes_gistemp_p4_fdr %>%
  select(-data) %>%
  unnest(sdif) %>%
  rename(chow_4 = chow,
         delta_4 = slope_delta,
         sig_4 = delta_sig)

clean_slopes_gistemp <- clean_slopes_gistemp_R %>%
  left_join(clean_slopes_gistemp_p2) %>%
  left_join(clean_slopes_gistemp_p3) %>%
  left_join(clean_slopes_gistemp_p4)

all_gistemp <- attr_slopes_gistemp %>%
  select(-data) %>%
  left_join(ipo_gistemp %>% select(-data)) %>%
  unnest(cipo) %>%
  rename(cor_ipo = cor,
         p_ipo = p) %>% 
  left_join(mei_gistemp %>% select(-data)) %>%
  unnest(cmei) %>% 
  rename(cor_mei = cor,
         p_mei = p) %>% 
  left_join(clean_slopes_gistemp)

## NOAA

clean_slopes_noaa_R <- slopes_noaa_R_fdr %>%
  select(-data) %>%
  unnest(sdif) %>%
  rename(chow_1 = chow,
         delta_1 = slope_delta,
         sig_1 = delta_sig)

clean_slopes_noaa_p2 <- slopes_noaa_p2_fdr %>%
  select(-data) %>%
  unnest(sdif) %>%
  rename(chow_2 = chow,
         delta_2 = slope_delta,
         sig_2 = delta_sig)

clean_slopes_noaa_p3 <- slopes_noaa_p3_fdr %>%
  select(-data) %>%
  unnest(sdif) %>%
  rename(chow_3 = chow,
         delta_3 = slope_delta,
         sig_3 = delta_sig)

clean_slopes_noaa_p4 <- slopes_noaa_p4_fdr %>%
  select(-data) %>%
  unnest(sdif) %>%
  rename(chow_4 = chow,
         delta_4 = slope_delta,
         sig_4 = delta_sig)

clean_slopes_noaa <- clean_slopes_noaa_R %>%
  left_join(clean_slopes_noaa_p2) %>%
  left_join(clean_slopes_noaa_p3) %>%
  left_join(clean_slopes_noaa_p4)

all_noaa <- attr_slopes_noaa %>%
  select(-data) %>%
  left_join(ipo_noaa %>% select(-data)) %>%
  unnest(cipo) %>%
  rename(cor_ipo = cor,
         p_ipo = p) %>% 
  left_join(mei_noaa %>% select(-data)) %>%
  unnest(cmei) %>% 
  rename(cor_mei = cor,
         p_mei = p) %>% 
  left_join(clean_slopes_noaa)

## HADCRUT

clean_slopes_hadcrut_R <- slopes_hadcrut_R_fdr %>%
  select(-data) %>%
  unnest(sdif) %>%
  rename(chow_1 = chow,
         delta_1 = slope_delta,
         sig_1 = delta_sig)

clean_slopes_hadcrut_p2 <- slopes_hadcrut_p2_fdr %>%
  select(-data) %>%
  unnest(sdif) %>%
  rename(chow_2 = chow,
         delta_2 = slope_delta,
         sig_2 = delta_sig)

clean_slopes_hadcrut_p3 <- slopes_hadcrut_p3_fdr %>%
  select(-data) %>%
  unnest(sdif) %>%
  rename(chow_3 = chow,
         delta_3 = slope_delta,
         sig_3 = delta_sig)

clean_slopes_hadcrut_p4 <- slopes_hadcrut_p4_fdr %>%
  select(-data) %>%
  unnest(sdif) %>%
  rename(chow_4 = chow,
         delta_4 = slope_delta,
         sig_4 = delta_sig)

clean_slopes_hadcrut <- clean_slopes_hadcrut_R %>%
  left_join(clean_slopes_hadcrut_p2) %>%
  left_join(clean_slopes_hadcrut_p3) %>%
  left_join(clean_slopes_hadcrut_p4)

all_hadcrut <- attr_slopes_hadcrut %>%
  select(-data) %>%
  left_join(ipo_hadcrut %>% select(-data)) %>%
  unnest(cipo) %>%
  rename(cor_ipo = cor,
         p_ipo = p) %>% 
  left_join(mei_hadcrut %>% select(-data)) %>%
  unnest(cmei) %>% 
  rename(cor_mei = cor,
         p_mei = p) %>% 
  left_join(clean_slopes_hadcrut)

### Global slopes ---------------------------------------------------

## just needed for cross-checking!

temp_gistemp_avg <- temp_gistemp %>%
  unnest(data) %>%
  group_by(year) %>%
  summarise(temp = mean(temp, na.rm = TRUE)) %>%
  summarise(
    slope_wp_1 = period_slope(tibble(year, temp), wp_1),
    slope_wp_2 = period_slope(tibble(year, temp), wp_2),
    slope_wp_3 = period_slope(tibble(year, temp), wp_3),
    slope_wp_4 = period_slope(tibble(year, temp), wp_4),
    slope_hp_1 = period_slope(tibble(year, temp), hp_1),
    slope_hp_2 = period_slope(tibble(year, temp), hp_2),
    slope_hp_3 = period_slope(tibble(year, temp), hp_3),
    slope_hp_4 = period_slope(tibble(year, temp), hp_4)
  )

temp_noaa_avg <- temp_noaa %>%
  unnest(data) %>%
  group_by(year) %>%
  summarise(temp = mean(temp, na.rm = TRUE)) %>%
  summarise(
    slope_wp_1 = period_slope(tibble(year, temp), wp_1),
    slope_wp_2 = period_slope(tibble(year, temp), wp_2),
    slope_wp_3 = period_slope(tibble(year, temp), wp_3),
    slope_wp_4 = period_slope(tibble(year, temp), wp_4),
    slope_hp_1 = period_slope(tibble(year, temp), hp_1),
    slope_hp_2 = period_slope(tibble(year, temp), hp_2),
    slope_hp_3 = period_slope(tibble(year, temp), hp_3),
    slope_hp_4 = period_slope(tibble(year, temp), hp_4)
  )

temp_hadcrut_avg <- temp_hadcrut %>%
  unnest(data) %>%
  group_by(year) %>%
  summarise(temp = mean(temp, na.rm = TRUE)) %>%
  summarise(
    slope_wp_1 = period_slope(tibble(year, temp), wp_1),
    slope_wp_2 = period_slope(tibble(year, temp), wp_2),
    slope_wp_3 = period_slope(tibble(year, temp), wp_3),
    slope_wp_4 = period_slope(tibble(year, temp), wp_4),
    slope_hp_1 = period_slope(tibble(year, temp), hp_1),
    slope_hp_2 = period_slope(tibble(year, temp), hp_2),
    slope_hp_3 = period_slope(tibble(year, temp), hp_3),
    slope_hp_4 = period_slope(tibble(year, temp), hp_4)
  )

### compute contribution according to Hankey ------------------------

hankey <- function(x) x/sum(x) * 100

## GISTEMP

slope_contrib_gistemp <- all_gistemp %>%
  mutate(
    slope_contrib_wp_1 = hankey(slope_wp_1),
    slope_contrib_wp_2 = hankey(slope_wp_2),
    slope_contrib_wp_3 = hankey(slope_wp_3),
    slope_contrib_wp_4 = hankey(slope_wp_4),
    slope_contrib_hp_1 = hankey(slope_hp_1),
    slope_contrib_hp_2 = hankey(slope_hp_2),
    slope_contrib_hp_3 = hankey(slope_hp_3),
    slope_contrib_hp_4 = hankey(slope_hp_4)
  )

slope_contrib_noaa <- all_noaa %>%
  mutate(
    slope_contrib_wp_1 = hankey(slope_wp_1),
    slope_contrib_wp_2 = hankey(slope_wp_2),
    slope_contrib_wp_3 = hankey(slope_wp_3),
    slope_contrib_wp_4 = hankey(slope_wp_4),
    slope_contrib_hp_1 = hankey(slope_hp_1),
    slope_contrib_hp_2 = hankey(slope_hp_2),
    slope_contrib_hp_3 = hankey(slope_hp_3),
    slope_contrib_hp_4 = hankey(slope_hp_4)
  )

slope_contrib_hadcrut <- all_hadcrut %>%
  mutate(
    slope_contrib_wp_1 = hankey(slope_wp_1),
    slope_contrib_wp_2 = hankey(slope_wp_2),
    slope_contrib_wp_3 = hankey(slope_wp_3),
    slope_contrib_wp_4 = hankey(slope_wp_4),
    slope_contrib_hp_1 = hankey(slope_hp_1),
    slope_contrib_hp_2 = hankey(slope_hp_2),
    slope_contrib_hp_3 = hankey(slope_hp_3),
    slope_contrib_hp_4 = hankey(slope_hp_4)
  )

### Analyses --------------------------------------------------------

### Contingency tables for IPO --------------------------------------

as_ctable <- function(df, .var = "count") {
  ## obey alphabetical order
  .rownames <- paste0(names(df)[1], c("FALSE", "TRUE"))
  .colnames <- paste0(names(df)[2], c("FALSE", "TRUE"))
  .df <- data.frame(
    matrix(df[[.var]], ncol = 2, byrow = TRUE)
  )
  rownames(.df) <- .rownames
  colnames(.df) <- .colnames
  t(.df)
}

## avoid repetition
conting_factors <- function(df) {
  df %>%
      mutate(
        sign_pos_ipo_fx = p_ipo < 0.05 & cor_ipo > 0,
        sign_pos_mei_fx = p_mei < 0.05 & cor_mei > 0,
        sldn_1 = slope_wp_1 > slope_hp_1,
        sldn_2 = slope_wp_2 > slope_hp_2,
        sldn_3 = slope_wp_3 > slope_hp_3,
        sldn_4 = slope_wp_4 > slope_hp_4,
        sign_sldn_mc_1 = slope_wp_1 > slope_hp_1 & sig_1 < 0.05,
        sign_sldn_mc_2 = slope_wp_2 > slope_hp_2 & sig_2 < 0.05,
        sign_sldn_mc_3 = slope_wp_3 > slope_hp_3 & sig_3 < 0.05,
        sign_sldn_mc_4 = slope_wp_4 > slope_hp_4 & sig_4 < 0.05,
        sign_sldn_ch_1 = slope_wp_1 > slope_hp_1 & chow_1 < 0.05,
        sign_sldn_ch_2 = slope_wp_2 > slope_hp_2 & chow_2 < 0.05,
        sign_sldn_ch_3 = slope_wp_3 > slope_hp_3 & chow_3 < 0.05,
        sign_sldn_ch_4 = slope_wp_4 > slope_hp_4 & chow_4 < 0.05
      )
}

## GISTEMP

cont_gistemp <- all_gistemp %>%
  conting_factors %>% 
  select(lon, lat, starts_with("sign_"), starts_with("sldn"))

## significant slowdown
## CAUTION: how to interpret this? we have a double criterion for the
## sign. slowdown, i.e. it is hard to pass the test!
(cont_gistemp %>% 
  group_by(sign_pos_ipo_fx, sign_sldn_ch_1) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_gistemp, 'sign_pos_ipo_fx', 'sign_sldn_mc_1', W = W)

## any slowdown

## GISTEMP/IPO

(cont_gistemp %>% 
  group_by(sign_pos_ipo_fx, sldn_1) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_gistemp, 'sign_pos_ipo_fx', 'sldn_1', W = W)

(cont_gistemp %>% 
  group_by(sign_pos_ipo_fx, sldn_2) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_gistemp, 'sign_pos_ipo_fx', 'sldn_2', W = W)

(cont_gistemp %>% 
  group_by(sign_pos_ipo_fx, sldn_3) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_gistemp, 'sign_pos_ipo_fx', 'sldn_3', W = W)

(cont_gistemp %>% 
  group_by(sign_pos_ipo_fx, sldn_4) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_gistemp, 'sign_pos_ipo_fx', 'sldn_4', W = W)

## NOAA

cont_noaa <- all_noaa %>%
  conting_factors %>% 
  select(lon, lat, starts_with("sign_"), starts_with("sldn"))

## any slowdown

## NOAA/IPO

(cont_noaa %>% 
  group_by(sign_pos_ipo_fx, sldn_1) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_noaa, 'sign_pos_ipo_fx', 'sldn_1', W = W)

(cont_noaa %>% 
  group_by(sign_pos_ipo_fx, sldn_2) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_noaa, 'sign_pos_ipo_fx', 'sldn_2', W = W)

(cont_noaa %>% 
  group_by(sign_pos_ipo_fx, sldn_3) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_noaa, 'sign_pos_ipo_fx', 'sldn_3', W = W)

(cont_noaa %>% 
  group_by(sign_pos_ipo_fx, sldn_4) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_noaa, 'sign_pos_ipo_fx', 'sldn_4', W = W)

## HADCRUT

cont_hadcrut <- all_hadcrut %>%
  conting_factors %>% 
  select(lon, lat, starts_with("sign_"), starts_with("sldn"))

## any slowdown

## HADCRUT/IPO

(cont_hadcrut %>% 
  group_by(sign_pos_ipo_fx, sldn_1) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_hadcrut, 'sign_pos_ipo_fx', 'sldn_1', W = W)

(cont_hadcrut %>% 
  group_by(sign_pos_ipo_fx, sldn_2) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_hadcrut, 'sign_pos_ipo_fx', 'sldn_2', W = W)

(cont_hadcrut %>% 
  group_by(sign_pos_ipo_fx, sldn_3) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_hadcrut, 'sign_pos_ipo_fx', 'sldn_3', W = W)

(cont_hadcrut %>% 
  group_by(sign_pos_ipo_fx, sldn_4) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_hadcrut, 'sign_pos_ipo_fx', 'sldn_4', W = W)

### Contingency tables for MEI --------------------------------------

## GISTEMP/IPO

## any slowdown

(cont_gistemp %>% 
  group_by(sign_pos_mei_fx, sldn_1) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_gistemp, 'sign_pos_mei_fx', 'sldn_1', W = W)

(cont_gistemp %>% 
  group_by(sign_pos_mei_fx, sldn_2) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_gistemp, 'sign_pos_mei_fx', 'sldn_2', W = W)

(cont_gistemp %>% 
  group_by(sign_pos_mei_fx, sldn_3) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_gistemp, 'sign_pos_mei_fx', 'sldn_3', W = W)

(cont_gistemp %>% 
  group_by(sign_pos_mei_fx, sldn_4) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_gistemp, 'sign_pos_mei_fx', 'sldn_4', W = W)

## significant slowdown (Chow)

(cont_gistemp %>% 
  group_by(sign_pos_mei_fx, sign_sldn_ch_1) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_gistemp, 'sign_pos_mei_fx', 'sign_sldn_ch_1', W
                 = W)

(cont_gistemp %>% 
  group_by(sign_pos_mei_fx, sign_sldn_ch_2) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_gistemp, 'sign_pos_mei_fx', 'sign_sldn_ch_2', W
                 = W)

(cont_gistemp %>% 
  group_by(sign_pos_mei_fx, sign_sldn_ch_3) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_gistemp, 'sign_pos_mei_fx', 'sign_sldn_ch_3', W
                 = W)

(cont_gistemp %>% 
  group_by(sign_pos_mei_fx, sign_sldn_ch_4) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_gistemp, 'sign_pos_mei_fx', 'sign_sldn_ch_4', W = W)

## NOAA

## any slowdown

cont_noaa <- all_noaa %>%
  conting_factors %>% 
  select(lon, lat, starts_with("sign_"), starts_with("sldn"))

## any slowdown

## NOAA/IPO

(cont_noaa %>% 
  group_by(sign_pos_mei_fx, sldn_1) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_noaa, 'sign_pos_mei_fx', 'sldn_1', W = W)

(cont_noaa %>% 
  group_by(sign_pos_mei_fx, sldn_2) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_noaa, 'sign_pos_mei_fx', 'sldn_2', W = W)

(cont_noaa %>% 
  group_by(sign_pos_mei_fx, sldn_3) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_noaa, 'sign_pos_mei_fx', 'sldn_3', W = W)

(cont_noaa %>% 
  group_by(sign_pos_mei_fx, sldn_4) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_noaa, 'sign_pos_mei_fx', 'sldn_4', W = W)

## HADCRUT

cont_hadcrut <- all_hadcrut %>%
  conting_factors %>% 
  select(lon, lat, starts_with("sign_"), starts_with("sldn"))

## any slowdown

## HADCRUT/IPO

(cont_hadcrut %>% 
  group_by(sign_pos_mei_fx, sldn_1) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_hadcrut, 'sign_pos_mei_fx', 'sldn_1', W = W)

(cont_hadcrut %>% 
  group_by(sign_pos_mei_fx, sldn_2) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_hadcrut, 'sign_pos_mei_fx', 'sldn_2', W = W)

(cont_hadcrut %>% 
  group_by(sign_pos_mei_fx, sldn_3) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_hadcrut, 'sign_pos_mei_fx', 'sldn_3', W = W)

(cont_hadcrut %>% 
  group_by(sign_pos_mei_fx, sldn_4) %>%
  summarise(count = n()) %>%
  as_ctable %T>%
  print %>% 
  chisq.test -> W)
mod_test_wrapper(cont_hadcrut, 'sign_pos_mei_fx', 'sldn_4', W = W)

### Contribution of IPO regions to hiatus effect ---------------

## Interesting question: how to set the regions?

## 1) IPO-Regions

## 1a) sign. (+) IPO influence

slope_contrib_gistemp %>%
  group_by(ipo_plus = cor_ipo > 0 & p_ipo < 0.05) %>%
  summarise(
    slope_contrib_wp_1 = sum(slope_contrib_wp_1),
    slope_contrib_wp_2 = sum(slope_contrib_wp_2),
    slope_contrib_wp_3 = sum(slope_contrib_wp_3),
    slope_contrib_wp_4 = sum(slope_contrib_wp_4),
    slope_contrib_hp_1 = sum(slope_contrib_hp_1),
    slope_contrib_hp_2 = sum(slope_contrib_hp_2),
    slope_contrib_hp_3 = sum(slope_contrib_hp_3),
    slope_contrib_hp_4 = sum(slope_contrib_hp_4)
  ) %>%
  gather(period, contrib, -ipo_plus) %>%
  filter(ipo_plus)

slope_contrib_hadcrut %>%
  group_by(ipo_plus = cor_ipo > 0 & p_ipo < 0.05) %>%
  summarise(
    slope_contrib_wp_1 = sum(slope_contrib_wp_1),
    slope_contrib_wp_2 = sum(slope_contrib_wp_2),
    slope_contrib_wp_3 = sum(slope_contrib_wp_3),
    slope_contrib_wp_4 = sum(slope_contrib_wp_4),
    slope_contrib_hp_1 = sum(slope_contrib_hp_1),
    slope_contrib_hp_2 = sum(slope_contrib_hp_2),
    slope_contrib_hp_3 = sum(slope_contrib_hp_3),
    slope_contrib_hp_4 = sum(slope_contrib_hp_4)
  ) %>%
  gather(period, contrib, -ipo_plus) %>%
  filter(ipo_plus)

slope_contrib_noaa %>%
  group_by(ipo_plus = cor_ipo > 0 & p_ipo < 0.05) %>%
  summarise(
    slope_contrib_wp_1 = sum(slope_contrib_wp_1),
    slope_contrib_wp_2 = sum(slope_contrib_wp_2),
    slope_contrib_wp_3 = sum(slope_contrib_wp_3),
    slope_contrib_wp_4 = sum(slope_contrib_wp_4),
    slope_contrib_hp_1 = sum(slope_contrib_hp_1),
    slope_contrib_hp_2 = sum(slope_contrib_hp_2),
    slope_contrib_hp_3 = sum(slope_contrib_hp_3),
    slope_contrib_hp_4 = sum(slope_contrib_hp_4)
  ) %>%
  gather(period, contrib, -ipo_plus) %>%
  filter(ipo_plus)

### Abuse data structure for plotting general trends ----------------

## restricted period w/ bootstrapped CI for weighted mean
boot_wmean_ci <- function(x, w, upper = TRUE) {
  n <- 1000
  means <- numeric(n)
  m <- length(x)
  for (i in 1:n) {
    the_sample <- sample(1:m, m, replace = TRUE)
    means[i] <- weighted.mean(x[the_sample], w[the_sample])
  }
  if (upper) {
    sort(means)[975]
  } else {
    sort(means)[25]
  }
}

all_gistemp2 <- left_join(all_gistemp, temp_gistemp)
ipo_trend_diff_gistemp %<f-% {
  all_gistemp2 %>%
    mutate(gcarea = tusk::gridcellarea(list(lon = lon, lat = lat), 0.5)) %>% 
    unnest(data) %>%
    mutate(ipo_plus = ifelse(cor_ipo > 0 & p_ipo < 0.05,
                             "Sign. positive IPO",
                             "Non-sign. or negative IPO")) %>% 
    group_by(ipo_plus, year) %>%
    summarise(
      meant = weighted.mean(temp, gcarea),
      ciu = boot_wmean_ci(temp, gcarea),
      cil = boot_wmean_ci(temp, gcarea, FALSE)
    ) %>%
    filter(year %in% 1972:2014) %>% 
    mutate(period = ifelse(year < 2001, "BP", "SP")) %>%
    mutate(dataset = "GISTEMP")
}

all_noaa2 <- left_join(all_noaa, temp_noaa)
ipo_trend_diff_noaa %<f-% {
  all_noaa2 %>%
    mutate(gcarea = tusk::gridcellarea(list(lon = lon, lat = lat), 2)) %>%   
    unnest(data) %>%
    mutate(ipo_plus = ifelse(cor_ipo > 0 & p_ipo < 0.05,
                             "Sign. positive IPO",
                             "Non-sign. or negative IPO")) %>% 
    group_by(ipo_plus, year) %>%
    summarise(
      meant = weighted.mean(temp, gcarea),
      ciu = boot_wmean_ci(temp, gcarea),
      cil = boot_wmean_ci(temp, gcarea, FALSE)
    ) %>%
    filter(year %in% 1972:2014) %>% 
    mutate(period = ifelse(year < 2001, "BP", "SP")) %>%
    mutate(dataset = "NOAA")
}

all_hadcrut2 <- left_join(all_hadcrut, temp_hadcrut)
ipo_trend_diff_hadcrut %<f-% {
  all_hadcrut2 %>%
    mutate(gcarea = tusk::gridcellarea(list(lon = lon, lat = lat), 2)) %>% 
    unnest(data) %>%
    mutate(ipo_plus = ifelse(cor_ipo > 0 & p_ipo < 0.05,
                             "Sign. positive IPO",
                             "Non-sign. or negative IPO")) %>% 
    group_by(ipo_plus, year) %>%
    summarise(
      meant = weighted.mean(temp, gcarea),
      ciu = boot_wmean_ci(temp, gcarea),
      cil = boot_wmean_ci(temp, gcarea, FALSE)
    ) %>%
    filter(year %in% 1972:2014) %>% 
    mutate(period = ifelse(year < 2001, "BP", "SP")) %>%
    mutate(dataset = "HADCRUT")
}

ipo_trend_diff <- rbind(
  ipo_trend_diff_gistemp,
  ipo_trend_diff_noaa,
  ipo_trend_diff_hadcrut
) %>% 
  mutate(
    dataset = ordered(dataset, levels = c("GISTEMP", "NOAA",
                                          "HADCRUT"),
                      labels = c("GISTEMP", "NOAAGlobalTemp", "HadCRUT"))
  )

ggplot(ipo_trend_diff, aes(x = year, y = meant, linetype = ipo_plus)) +
  geom_ribbon(aes(x = year, ymax = ciu, ymin = cil), fill = "lightgrey") + 
  geom_line(colour = "black") +
  geom_smooth(aes(colour = period, fill = period, linetype = ipo_plus),
              method = "lm", se = TRUE) +
  scale_colour_manual(values = rep("#b70303", 2), guide = FALSE) +
  scale_fill_manual(values = c("#ff000000", 2), guide = FALSE) +
  scale_linetype_discrete(name = "") +
  theme_bw() +
  ylab("Temperature anomaly (°C)") +
  xlab("Years") +
  facet_grid(dataset ~ .) +
  theme(legend.position="bottom")

ggsave("figures/ipo_trend_diff_restricted_p1.png", width = 8, height = 4)
ggsave("figures/ipo_trend_diff_restricted_p1.pdf", width = 8, height = 4)

## same for p2
all_gistemp2 %>%
  mutate(gcarea = tusk::gridcellarea(list(lon = lon, lat = lat), 0.5)) %>% 
  unnest(data) %>%
  mutate(ipo_plus = ifelse(cor_ipo > 0 & p_ipo < 0.05,
                           "Sign. positive IPO",
                           "Non-sign. or negative IPO")) %>% 
  group_by(ipo_plus, year) %>%
  summarise(
    meant = weighted.mean(temp, gcarea),
    ciu = boot_wmean_ci(temp, gcarea),
    cil = boot_wmean_ci(temp, gcarea, FALSE)
  ) %>%
  filter(year %in% 1972:2014) %>% 
  mutate(period = ifelse(year < 2000, "BP", "SP")) %>%
  mutate(dataset = "GISTEMP") -> ipo_trend_diff_gistemp

all_noaa2 %>%
  mutate(gcarea = tusk::gridcellarea(list(lon = lon, lat = lat), 2)) %>%   
  unnest(data) %>%
  mutate(ipo_plus = ifelse(cor_ipo > 0 & p_ipo < 0.05,
                           "Sign. positive IPO",
                           "Non-sign. or negative IPO")) %>% 
  group_by(ipo_plus, year) %>%
  summarise(
    meant = weighted.mean(temp, gcarea),
    ciu = boot_wmean_ci(temp, gcarea),
    cil = boot_wmean_ci(temp, gcarea, FALSE)
  ) %>%
  filter(year %in% 1972:2014) %>% 
  mutate(period = ifelse(year < 2000, "BP", "SP")) %>%
  mutate(dataset = "NOAA") -> ipo_trend_diff_noaa

all_hadcrut2 %>%
  mutate(gcarea = tusk::gridcellarea(list(lon = lon, lat = lat), 2)) %>% 
  unnest(data) %>%
  mutate(ipo_plus = ifelse(cor_ipo > 0 & p_ipo < 0.05,
                           "Sign. positive IPO",
                           "Non-sign. or negative IPO")) %>% 
  group_by(ipo_plus, year) %>%
  summarise(
    meant = weighted.mean(temp, gcarea),
    ciu = boot_wmean_ci(temp, gcarea),
    cil = boot_wmean_ci(temp, gcarea, FALSE)
  ) %>%
  filter(year %in% 1972:2014) %>% 
  mutate(period = ifelse(year < 2000, "BP", "SP")) %>%
  mutate(dataset = "HADCRUT") -> ipo_trend_diff_hadcrut

ipo_trend_diff <- rbind(
  ipo_trend_diff_gistemp,
  ipo_trend_diff_noaa,
  ipo_trend_diff_hadcrut
) %>% 
  mutate(
    dataset = ordered(dataset, levels = c("GISTEMP", "NOAA",
                                          "HADCRUT"),
                      labels = c("GISTEMP", "NOAAGlobalTemp", "HadCRUT"))
  )

ggplot(ipo_trend_diff, aes(x = year, y = meant, linetype = ipo_plus)) +
  geom_ribbon(aes(x = year, ymax = ciu, ymin = cil), fill = "lightgrey") + 
  geom_line(colour = "black") +
  geom_smooth(aes(colour = period, fill = period, linetype = ipo_plus),
              method = "lm", se = TRUE) +
  scale_colour_manual(values = rep("#b70303", 2), guide = FALSE) +
  scale_fill_manual(values = c("#ff000000", 2), guide = FALSE) +
  scale_linetype_discrete(name = "") +
  theme_bw() +
  ylab("Temperature anomaly (°C)") +
  xlab("Years") +
  facet_grid(dataset ~ .) +
  theme(legend.position="bottom")

ggsave("figures/ipo_trend_diff_restricted_p2.png", width = 8, height = 4)
ggsave("figures/ipo_trend_diff_restricted_p2.pdf", width = 8, height = 4)

## same for p3
all_gistemp2 %>%
  mutate(gcarea = tusk::gridcellarea(list(lon = lon, lat = lat), 0.5)) %>% 
  unnest(data) %>%
  mutate(ipo_plus = ifelse(cor_ipo > 0 & p_ipo < 0.05,
                           "Sign. positive IPO",
                           "Non-sign. or negative IPO")) %>% 
  group_by(ipo_plus, year) %>%
  summarise(
    meant = weighted.mean(temp, gcarea),
    ciu = boot_wmean_ci(temp, gcarea),
    cil = boot_wmean_ci(temp, gcarea, FALSE)
  ) %>%
  filter(year %in% 1972:2013) %>% 
  mutate(period = ifelse(year < 1999, "BP", "SP")) %>%
  mutate(dataset = "GISTEMP") -> ipo_trend_diff_gistemp

all_noaa2 %>%
  mutate(gcarea = tusk::gridcellarea(list(lon = lon, lat = lat), 2)) %>%   
  unnest(data) %>%
  mutate(ipo_plus = ifelse(cor_ipo > 0 & p_ipo < 0.05,
                           "Sign. positive IPO",
                           "Non-sign. or negative IPO")) %>% 
  group_by(ipo_plus, year) %>%
  summarise(
    meant = weighted.mean(temp, gcarea),
    ciu = boot_wmean_ci(temp, gcarea),
    cil = boot_wmean_ci(temp, gcarea, FALSE)
  ) %>%
  filter(year %in% 1972:2013) %>% 
  mutate(period = ifelse(year < 1999, "BP", "SP")) %>%
  mutate(dataset = "NOAA") -> ipo_trend_diff_noaa

all_hadcrut2 %>%
  mutate(gcarea = tusk::gridcellarea(list(lon = lon, lat = lat), 2)) %>% 
  unnest(data) %>%
  mutate(ipo_plus = ifelse(cor_ipo > 0 & p_ipo < 0.05,
                           "Sign. positive IPO",
                           "Non-sign. or negative IPO")) %>% 
  group_by(ipo_plus, year) %>%
  summarise(
    meant = weighted.mean(temp, gcarea),
    ciu = boot_wmean_ci(temp, gcarea),
    cil = boot_wmean_ci(temp, gcarea, FALSE)
  ) %>%
  filter(year %in% 1972:2013) %>% 
  mutate(period = ifelse(year < 1999, "BP", "SP")) %>%
  mutate(dataset = "HADCRUT") -> ipo_trend_diff_hadcrut

ipo_trend_diff <- rbind(
  ipo_trend_diff_gistemp,
  ipo_trend_diff_noaa,
  ipo_trend_diff_hadcrut
) %>% 
  mutate(
    dataset = ordered(dataset, levels = c("GISTEMP", "NOAA",
                                          "HADCRUT"),
                      labels = c("GISTEMP", "NOAAGlobalTemp", "HadCRUT"))
  )

ggplot(ipo_trend_diff, aes(x = year, y = meant, linetype = ipo_plus)) +
  geom_ribbon(aes(x = year, ymax = ciu, ymin = cil), fill = "lightgrey") + 
  geom_line(colour = "black") +
  geom_smooth(aes(colour = period, fill = period, linetype = ipo_plus),
              method = "lm", se = TRUE) +
  scale_colour_manual(values = rep("#b70303", 2), guide = FALSE) +
  scale_fill_manual(values = c("#ff000000", 2), guide = FALSE) +
  scale_linetype_discrete(name = "") +
  theme_bw() +
  ylab("Temperature anomaly (°C)") +
  xlab("Years") +
  facet_grid(dataset ~ .) +
  theme(legend.position="bottom")

ggsave("figures/ipo_trend_diff_restricted_p3.pdf", width = 8, height = 4)
ggsave("figures/ipo_trend_diff_restricted_p3.png", width = 8, height = 4)

## same for p4
all_gistemp2 %>%
  mutate(gcarea = tusk::gridcellarea(list(lon = lon, lat = lat), 0.5)) %>% 
  unnest(data) %>%
  mutate(ipo_plus = ifelse(cor_ipo > 0 & p_ipo < 0.05,
                           "Sign. positive IPO",
                           "Non-sign. or negative IPO")) %>% 
  group_by(ipo_plus, year) %>%
  summarise(
    meant = weighted.mean(temp, gcarea),
    ciu = boot_wmean_ci(temp, gcarea),
    cil = boot_wmean_ci(temp, gcarea, FALSE)
  ) %>%
  filter(year %in% 1972:2012) %>% 
  mutate(period = ifelse(year < 1998, "BP", "SP")) %>%
  mutate(dataset = "GISTEMP") -> ipo_trend_diff_gistemp

all_noaa2 %>%
  mutate(gcarea = tusk::gridcellarea(list(lon = lon, lat = lat), 2)) %>%   
  unnest(data) %>%
  mutate(ipo_plus = ifelse(cor_ipo > 0 & p_ipo < 0.05,
                           "Sign. positive IPO",
                           "Non-sign. or negative IPO")) %>% 
  group_by(ipo_plus, year) %>%
  summarise(
    meant = weighted.mean(temp, gcarea),
    ciu = boot_wmean_ci(temp, gcarea),
    cil = boot_wmean_ci(temp, gcarea, FALSE)
  ) %>%
  filter(year %in% 1972:2012) %>% 
  mutate(period = ifelse(year < 1998, "BP", "SP")) %>%
  mutate(dataset = "NOAA") -> ipo_trend_diff_noaa

all_hadcrut2 %>%
  mutate(gcarea = tusk::gridcellarea(list(lon = lon, lat = lat), 2)) %>% 
  unnest(data) %>%
  mutate(ipo_plus = ifelse(cor_ipo > 0 & p_ipo < 0.05,
                           "Sign. positive IPO",
                           "Non-sign. or negative IPO")) %>% 
  group_by(ipo_plus, year) %>%
  summarise(
    meant = weighted.mean(temp, gcarea),
    ciu = boot_wmean_ci(temp, gcarea),
    cil = boot_wmean_ci(temp, gcarea, FALSE)
  ) %>%
  filter(year %in% 1972:2012) %>% 
  mutate(period = ifelse(year < 1998, "BP", "SP")) %>%
  mutate(dataset = "HADCRUT") -> ipo_trend_diff_hadcrut

ipo_trend_diff <- rbind(
  ipo_trend_diff_gistemp,
  ipo_trend_diff_noaa,
  ipo_trend_diff_hadcrut
) %>% 
  mutate(
    dataset = ordered(dataset, levels = c("GISTEMP", "NOAA",
                                          "HADCRUT"),
                      labels = c("GISTEMP", "NOAAGlobalTemp", "HadCRUT"))
  )

ggplot(ipo_trend_diff, aes(x = year, y = meant, linetype = ipo_plus)) +
  geom_ribbon(aes(x = year, ymax = ciu, ymin = cil), fill = "lightgrey") + 
  geom_line(colour = "black") +
  geom_smooth(aes(colour = period, fill = period, linetype = ipo_plus),
              method = "lm", se = TRUE) +
  scale_colour_manual(values = rep("#b70303", 2), guide = FALSE) +
  scale_fill_manual(values = c("#ff000000", 2), guide = FALSE) +
  scale_linetype_discrete(name = "") +
  theme_bw() +
  ylab("Temperature anomaly (°C)") +
  xlab("Years") +
  facet_grid(dataset ~ .) +
  theme(legend.position="bottom")

ggsave("figures/ipo_trend_diff_restricted_p4.pdf", width = 8, height = 4)
ggsave("figures/ipo_trend_diff_restricted_p4.png", width = 8, height = 4)
