request_all("slopes_")

perc_func <- function(x) {
  x %>%
    unnest(sdif) %>%
    mutate(
      strong_mc = ifelse(slope_delta < 0 & delta_sig < 0.05,
                         "sign. Hiatus", ifelse(
                           slope_delta > 0 & delta_sig < 0.05,
                           "sign. Antihiatus", "Something else"
                         )),
      weak = ifelse(slope_delta < 0, "Hiatus", "Antihiatus"),
      strong_chow = ifelse(slope_delta < 0 & chow < 0.05,
                         "sign. Hiatus", ifelse(
                           slope_delta > 0 & chow < 0.05,
                           "sign. Antihiatus", "Something else"
                         ))) -> x
  x %>% group_by(strong_mc) %>% summarise(n() * 100 / nrow(x)) %>% print
  x %>% group_by(strong_chow) %>% summarise(n() * 100 /nrow(x)) %>% print
  x %>% group_by(weak) %>% summarise(n() * 100 / nrow(x)) %>% print

  invisible(NULL)
}

cat("gistemp R \n")
slopes_gistemp_R_fdr %>% perc_func %>% print
cat("\n\ngistemp p2 \n")
slopes_gistemp_p2_fdr %>% perc_func %>% print
cat("\n\ngistemp p3 \n")
slopes_gistemp_p3_fdr %>% perc_func %>% print
cat("\n\ngistemp p4 \n")
slopes_gistemp_p4_fdr %>% perc_func %>% print

cat("\n\nnoaa R \n")
slopes_noaa_R_fdr %>% perc_func %>% print
cat("\n\nnoaa p2 \n")
slopes_noaa_p2_fdr %>% perc_func %>% print
cat("\n\nnoaa p3 \n")
slopes_noaa_p3_fdr %>% perc_func %>% print
cat("\n\nnoaa p4 \n")
slopes_noaa_p4_fdr %>% perc_func %>% print

cat("\n\nhadcrut R \n")
slopes_hadcrut_R_fdr %>% perc_func %>% print
cat("\n\nhadcrut p2 \n")
slopes_hadcrut_p2_fdr %>% perc_func %>% print
cat("\n\nhadcrut p3 \n")
slopes_hadcrut_p3_fdr %>% perc_func %>% print
cat("\n\nhadcrut p4 \n")
slopes_hadcrut_p4_fdr %>% perc_func %>% print
