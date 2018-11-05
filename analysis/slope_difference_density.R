## compare slope changes between different periods w/ and w/o el nino
## interpolation

request_all("slopes_")

extract_sdif <- function(df, .period) {
  df %>%
    unnest(sdif) %>%
    select(lon, lat, slope_delta) %>%
    rename(!!.period := slope_delta)
}

wilcox_tests <- function(df) {
  list(
    wilcox.test(df$R, df$p2, paired = TRUE),
    wilcox.test(df$R, df$p3, paired = TRUE),
    wilcox.test(df$R, df$p4, paired = TRUE),
    wilcox.test(df$p2, df$p3, paired = TRUE),
    wilcox.test(df$p2, df$p4, paired = TRUE),
    wilcox.test(df$p3, df$p4, paired = TRUE)
  )
}

all_diff_gistemp <- slopes_gistemp_R %>%
  extract_sdif("R") %>%
  left_join(slopes_gistemp_p2 %>% extract_sdif("p2")) %>%
  left_join(slopes_gistemp_p3 %>% extract_sdif("p3")) %>%
  left_join(slopes_gistemp_p4 %>% extract_sdif("p4"))

cat("Pairwise Wilcoxon tests for more positive slope changes in alternative periods:\n")
all_diff_gistemp %>% wilcox_tests

all_diff_gistemp <- slopes_gistemp_R %>%
  extract_sdif("R") %>%
  left_join(slopes_gistemp_p2 %>% extract_sdif("p2")) %>%
  left_join(slopes_gistemp_p3 %>% extract_sdif("p3")) %>%
  left_join(slopes_gistemp_p4 %>% extract_sdif("p4")) %>% 
  mutate(diffRp2 = p2 - R,
         diffRp3 = p3 - R,
         diffRp4 = p4 - R) %>%
  select(lon, lat, starts_with("diff")) %>% 
  gather(difference, value, starts_with("diff")) %>%
  mutate(dataset = "GISTEMP")

## same for NOAA & HadCRUT
all_diff_noaa <- slopes_noaa_R %>%
  extract_sdif("R") %>%
  left_join(slopes_noaa_p2 %>% extract_sdif("p2")) %>%
  left_join(slopes_noaa_p3 %>% extract_sdif("p3")) %>%
  left_join(slopes_noaa_p4 %>% extract_sdif("p4")) %>%
  mutate(diffRp2 = p2 - R,
         diffRp3 = p3 - R,
         diffRp4 = p4 - R) %>%
  select(lon, lat, starts_with("diff")) %>% 
  gather(difference, value, starts_with("diff")) %>%
  mutate(dataset = "NOAAGlobalTemp")

all_diff_hadcrut <- slopes_hadcrut_R %>%
  extract_sdif("R") %>%
  left_join(slopes_hadcrut_p2 %>% extract_sdif("p2")) %>%
  left_join(slopes_hadcrut_p3 %>% extract_sdif("p3")) %>%
  left_join(slopes_hadcrut_p4 %>% extract_sdif("p4")) %>%
  mutate(diffRp2 = p2 - R,
         diffRp3 = p3 - R,
         diffRp4 = p4 - R) %>%
  select(lon, lat, starts_with("diff")) %>% 
  gather(difference, value, starts_with("diff")) %>%
  mutate(dataset = "HadCRUT")

all_diff <- rbind(all_diff_gistemp,
                 all_diff_noaa,
                 all_diff_hadcrut)

all_diff$dataset <- ordered(all_diff$dataset,
                           levels = c("GISTEMP", "NOAAGlobalTemp",
                                      "HadCRUT"))
all_diff$difference <- ordered(all_diff$difference,
                             levels = c("diffRp2", "diffRp3",
                                        "diffRp4"),
                             labels =
                               c("(1972-1999|2000-2014) - (1972-2000|2001-2014)",
                                 "(1972-1998|1999-2013) - (1972-2000|2001-2014)",
                                 "(1972-1997|1998-2012) - (1972-2000|2001-2014)"))

ggplot(all_diff, aes(x = value)) +
  geom_density(aes(fill = difference), alpha = 0.5) +
  facet_grid(dataset ~ .) +
  xlab("Difference in slope differences for periods") +
  ylab("Density") +
  scale_fill_discrete(name = "Periods for differences") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "vertical")

ggsave("figures/slope_changes_density.pdf", width = 5, height = 5)
ggsave("figures/slope_changes_density.png", width = 5, height = 5)
ggsave("figures/slope_changes_density.tiff", width = 5, height = 5,
       dpi = 1200, compression = "lzw")
