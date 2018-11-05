## percentage of grid cells with positive temp/mei correlation

request("mei_gistemp")
request("mei_noaa")
request("mei_hadcrut")
request_all("slopes_")

mei_percent <- function(x) {
  x %>%
    unnest(cmei) %>%
    mutate(sig_pos = cor > 0 & p < 0.05) %>%
    group_by(sig_pos) %>%
    summarise(n() * 100 / nrow(x))
}

mei_slowd_percent <- function(x, y) {
  x %>%
    select(-data) %>% 
    left_join(y %>% select(-data)) %>% 
    unnest(cmei) %>%
    unnest(sdif) %>% 
    mutate(sig_pos = cor > 0 & p < 0.05,
           sig_slow = slope_delta < 0 & delta_sig < 0.05) %>%
    group_by(sig_pos & sig_slow) %>%
    summarise(n() * 100 / nrow(x))
}

mei_gistemp %>% mei_percent
mei_noaa %>% mei_percent
mei_hadcrut %>% mei_percent

cat("hadcrut period R\n")
mei_hadcrut %>% mei_slowd_percent(slopes_hadcrut_R) %>% print
cat("\n\nhadcrut period 2\n")
mei_hadcrut %>% mei_slowd_percent(slopes_hadcrut_p2) %>% print
cat("\n\nhadcrut period 3\n")
mei_hadcrut %>% mei_slowd_percent(slopes_hadcrut_p3) %>% print
cat("\n\nhadcrut period 4\n")
mei_hadcrut %>% mei_slowd_percent(slopes_hadcrut_p4) %>% print

cat("gistemp period R\n")
mei_gistemp %>% mei_slowd_percent(slopes_gistemp_R) %>% print
cat("\n\ngistemp period 2\n")
mei_gistemp %>% mei_slowd_percent(slopes_gistemp_p2) %>% print
cat("\n\ngistemp period 3\n")
mei_gistemp %>% mei_slowd_percent(slopes_gistemp_p3) %>% print
cat("\n\ngistemp period 4\n")
mei_gistemp %>% mei_slowd_percent(slopes_gistemp_p4) %>% print

cat("noaa period R\n")
mei_noaa %>% mei_slowd_percent(slopes_noaa_R) %>% print
cat("\n\nnoaa period 2\n")
mei_noaa %>% mei_slowd_percent(slopes_noaa_p2) %>% print
cat("\n\nnoaa period 3\n")
mei_noaa %>% mei_slowd_percent(slopes_noaa_p3) %>% print
cat("\n\nnoaa period 4\n")
mei_noaa %>% mei_slowd_percent(slopes_noaa_p4) %>% print
