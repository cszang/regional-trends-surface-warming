# adjusts slopes p-values for multiple testing
# for all slope_ objects

fdr_fn_slopes <- function(.slopes) {
  p_delta <- .slopes %>% unnest(sdif) %>% .$delta_sig %>% fdr_fn
  .slopes$chow <- .slopes$chow %>% fdr_fn 
  for (i in 1:nrow(.slopes)) .slopes$sdif[[i]]$delta_sig <- p_delta[i]
  .slopes
}

slopes_gistemp_R_fdr %<f-% { slopes_gistemp_R %>% fdr_fn_slopes }
slopes_gistemp_p2_fdr %<f-% { slopes_gistemp_p2 %>% fdr_fn_slopes }
slopes_gistemp_p3_fdr %<f-% { slopes_gistemp_p3 %>% fdr_fn_slopes }
slopes_gistemp_p4_fdr %<f-% { slopes_gistemp_p4 %>% fdr_fn_slopes }

slopes_hadcrut_R_fdr %<f-% { slopes_hadcrut_R %>% fdr_fn_slopes }
slopes_hadcrut_p2_fdr %<f-% { slopes_hadcrut_p2 %>% fdr_fn_slopes }
slopes_hadcrut_p3_fdr %<f-% { slopes_hadcrut_p3 %>% fdr_fn_slopes }
slopes_hadcrut_p4_fdr %<f-% { slopes_hadcrut_p4 %>% fdr_fn_slopes }

slopes_noaa_R_fdr %<f-% { slopes_noaa_R %>% fdr_fn_slopes }
slopes_noaa_p2_fdr %<f-% { slopes_noaa_p2 %>% fdr_fn_slopes }
slopes_noaa_p3_fdr %<f-% { slopes_noaa_p3 %>% fdr_fn_slopes }
slopes_noaa_p4_fdr %<f-% { slopes_noaa_p4 %>% fdr_fn_slopes }

slopes_meiresid_gistemp_R_fdr %<f-% { slopes_meiresid_gistemp_R %>% fdr_fn_slopes }
slopes_meiresid_gistemp_p2_fdr %<f-% { slopes_meiresid_gistemp_p2 %>% fdr_fn_slopes }
slopes_meiresid_gistemp_p3_fdr %<f-% { slopes_meiresid_gistemp_p3 %>% fdr_fn_slopes }
slopes_meiresid_gistemp_p4_fdr %<f-% { slopes_meiresid_gistemp_p4 %>% fdr_fn_slopes }

slopes_meiresid_hadcrut_R_fdr %<f-% { slopes_meiresid_hadcrut_R %>% fdr_fn_slopes }
slopes_meiresid_hadcrut_p2_fdr %<f-% { slopes_meiresid_hadcrut_p2 %>% fdr_fn_slopes }
slopes_meiresid_hadcrut_p3_fdr %<f-% { slopes_meiresid_hadcrut_p3 %>% fdr_fn_slopes }
slopes_meiresid_hadcrut_p4_fdr %<f-% { slopes_meiresid_hadcrut_p4 %>% fdr_fn_slopes }

slopes_meiresid_noaa_R_fdr %<f-% { slopes_meiresid_noaa_R %>% fdr_fn_slopes }
slopes_meiresid_noaa_p2_fdr %<f-% { slopes_meiresid_noaa_p2 %>% fdr_fn_slopes }
slopes_meiresid_noaa_p3_fdr %<f-% { slopes_meiresid_noaa_p3 %>% fdr_fn_slopes }
slopes_meiresid_noaa_p4_fdr %<f-% { slopes_meiresid_noaa_p4 %>% fdr_fn_slopes }