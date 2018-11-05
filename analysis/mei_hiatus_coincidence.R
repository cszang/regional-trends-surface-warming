## map of spatial agreement of slowdown with sign. IPO correlation

request_all("ipo_")
request_all("slopes_")

## x: one of slopes_[dataset]_[periods]_fdr
## y: one of ipo_[dataset]_fdr
make_trend_combinations <- function(x, y) {
  x %>%
    unnest(sdif) %>%
    select(-data) %>%
    rename(p_trend = chow) %>% 
    left_join(y %>%
                unnest(cipo) %>%
                select(-data) %>%
                rename(p_ipo = p,
                       cor_ipo = cor)) %>%
    mutate(
      slowdown = ifelse(slope_delta < 0, TRUE, FALSE),
      p_trend = ifelse(p_trend < 0.05, TRUE, FALSE),
      pos_ipo = ifelse(cor_ipo > 0, TRUE, FALSE),
      p_ipo = ifelse(p_ipo < 0.05, TRUE, FALSE)
    ) %>%
    mutate(
      ## 4x4 matrix of possible combinations:
      ## |                    | sign. + IPO | + IPO | - IPO | sign. - IPO |
      ## | sign. slowdown     |           1 |     2 |     3 |           4 |
      ## | slowdown           |           5 |     6 |     7 |           8 |
      ## | acceleration       |           9 |    10 |    11 |          12 |
      ## | sign. acceleration |          13 |    14 |    15 |          16 |
      combination = as.factor(ifelse(slowdown,
                                     ifelse(p_trend,
                                            ifelse(pos_ipo, # significant slowdown
                                                   ifelse(p_ipo, 1, 2),
                                                   ifelse(p_ipo, 4, 3)
                                                   ),
                                            ifelse(pos_ipo, # non-sign. slowdown
                                                   ifelse(p_ipo, 5, 6),
                                                   ifelse(p_ipo, 8, 7)
                                                   )),
                                     ifelse(p_trend, 
                                            ifelse(pos_ipo, # significant acceleration
                                                   ifelse(p_ipo, 13, 14),
                                                   ifelse(p_ipo, 16, 15)
                                                   ),
                                            ifelse(pos_ipo, # non-sign. acceleration
                                                   ifelse(p_ipo, 9, 10),
                                                   ifelse(p_ipo, 12, 11)
                                                   ))))
    ) %>%
    select(lon, lat, combination)
}

table_stats <- function(x) {
  tab <- x$combination %>% table
  all_slowdown <- sum(tab[as.character(c(1:8))], na.rm = TRUE)
  all_acc <- sum(tab[as.character(c(9:16))], na.rm = TRUE)
  sig_slowdown <- sum(tab[as.character(c(1:4))], na.rm = TRUE)
  sig_acc <- sum(tab[as.character(c(13:16))], na.rm = TRUE)
  perc_pos_ipo_all_slowdown <- sum(tab[as.character(c(1, 5))]) * 100 / all_slowdown
  perc_neg_ipo_all_acc <- sum(tab[as.character(c(12, 16))]) * 100 / all_acc
  perc_pos_ipo_sig_slowdown <- sum(tab[as.character(c(1))]) * 100 / sig_slowdown
  perc_neg_ipo_sig_acc <- sum(tab[as.character(c(16))]) * 100 / sig_acc
  cat("all slowdown & + IPO:", perc_pos_ipo_all_slowdown, "\n")
  cat("sig. slowdown & + IPO:", perc_pos_ipo_sig_slowdown, "\n")
  cat("all acc & - IPO:", perc_neg_ipo_all_acc, "\n")
  cat("sig. acc & - IPO:", perc_neg_ipo_sig_acc, "\n")
}

make_raster_object <- function(x, noaa = FALSE) {
  noaa_fn_closure <- function(noaa) {
    if (noaa) {
      function(x) {
        mutate(x, lon = ifelse(lon < 180, lon, lon - 360))
      }
    } else {
      function(x) x
    }
  }
  noaa_fn <- noaa_fn_closure(noaa)
  x %>%
    noaa_fn %>% 
    rename(x = lon, y = lat) %>%
    reraster
}

make_map_layer <- function(x) {
  tm_shape(x) +
    tm_raster("combination", palette = adaptive_palette(x, palette16),
              legend.show = FALSE, auto.palette.mapping = FALSE,
              colorNA = "#e3e3e3", max.categories = 16, n = 16) +
  map_layer
}

## corresponding to 4x4 matrix
palette16 <- c(
  "#2A4A36",                 
  "#8F9D4A",
  "#D1E56B",                                        
  "#F0F133",                                        
  "#50774E",
  "#BDC887",
  "#E5ECB0",                                        
  "#E9AF2A",
  "#456DAF",                                        
  "#A198CC",
  "#E5A28C",
  "#DF581E",                                        
  "#294596",
  "#5D366F",
  "#A4243B",
  "#D91615"
)

make_trend_combinations(slopes_gistemp_R_fdr, ipo_gistemp_fdr) %>%
  table_stats
make_trend_combinations(slopes_gistemp_p2_fdr, ipo_gistemp_fdr) %>%
  table_stats
make_trend_combinations(slopes_gistemp_p3_fdr, ipo_gistemp_fdr) %>%
  table_stats
make_trend_combinations(slopes_gistemp_p3_fdr, ipo_gistemp_fdr) %>%
  table_stats

make_trend_combinations(slopes_noaa_R_fdr, ipo_noaa_fdr) %>%
  table_stats
make_trend_combinations(slopes_noaa_p2_fdr, ipo_noaa_fdr) %>%
  table_stats
make_trend_combinations(slopes_noaa_p3_fdr, ipo_noaa_fdr) %>%
  table_stats
make_trend_combinations(slopes_noaa_p3_fdr, ipo_noaa_fdr) %>%
  table_stats

make_trend_combinations(slopes_hadcrut_R_fdr, ipo_hadcrut_fdr) %>%
  table_stats
make_trend_combinations(slopes_hadcrut_p2_fdr, ipo_hadcrut_fdr) %>%
  table_stats
make_trend_combinations(slopes_hadcrut_p3_fdr, ipo_hadcrut_fdr) %>%
  table_stats
make_trend_combinations(slopes_hadcrut_p3_fdr, ipo_hadcrut_fdr) %>%
  table_stats

## /DEBUG
x <- make_trend_combinations(slopes_gistemp_R_fdr, ipo_gistemp_fdr)
.raster <- x %>%
  make_raster_object
.palette <- palette16
ap <- adaptive_palette(.raster, .palette)
.x <- x %>% mutate(col = .palette[x$combination])
plot(.x$lon, .x$lat, col = .x$col, pch = 20)

make_trend_combinations(slopes_gistemp_R_fdr, ipo_gistemp_fdr) %>%
    make_raster_object %>%
    make_map_layer
## /DEBUG

pdf("figures/ipo_hiatus_agreement.pdf", width = 8, height = 7)
tmap_arrange(
  ## R
  make_trend_combinations(slopes_gistemp_R_fdr, ipo_gistemp_fdr) %>%
    make_raster_object %>%
    make_map_layer,
  make_trend_combinations(slopes_noaa_R_fdr, ipo_noaa_fdr) %>%
    make_raster_object(noaa = TRUE) %>%
    make_map_layer,
  make_trend_combinations(slopes_hadcrut_R_fdr, ipo_hadcrut_fdr) %>%
    make_raster_object %>%
    make_map_layer,

  ## p2
  make_trend_combinations(slopes_gistemp_p2_fdr, ipo_gistemp_fdr) %>%
    make_raster_object %>%
    make_map_layer,
  make_trend_combinations(slopes_noaa_p2_fdr, ipo_noaa_fdr) %>%
    make_raster_object(noaa = TRUE) %>%
    make_map_layer,
  make_trend_combinations(slopes_hadcrut_p2_fdr, ipo_hadcrut_fdr) %>%
    make_raster_object %>%
    make_map_layer,

  ## p3
  make_trend_combinations(slopes_gistemp_p3_fdr, ipo_gistemp_fdr) %>%
    make_raster_object %>%
    make_map_layer,
  make_trend_combinations(slopes_noaa_p3_fdr, ipo_noaa_fdr) %>%
    make_raster_object(noaa = TRUE) %>%
    make_map_layer,
  make_trend_combinations(slopes_hadcrut_p3_fdr, ipo_hadcrut_fdr) %>%
    make_raster_object %>%
    make_map_layer,

  ## p4
  make_trend_combinations(slopes_gistemp_p4_fdr, ipo_gistemp_fdr) %>%
    make_raster_object %>%
    make_map_layer,
  make_trend_combinations(slopes_noaa_p4_fdr, ipo_noaa_fdr) %>%
    make_raster_object(noaa = TRUE) %>%
    make_map_layer,
  make_trend_combinations(slopes_hadcrut_p4_fdr, ipo_hadcrut_fdr) %>%
    make_raster_object %>%
    make_map_layer,
  nrow = 4
)
dev.off()

## make raw version of legend
trend_labels <- c("Sign. acceleration", "Acceleration", "Slowdown",
                  "Sign. slowdown")
ipo_labels <- c("Sign. pos. corr.", "Pos. corr.", "Neg. corr.",
                 "Sign. neg. corr.")
g <- seq(-1.5, 1.5, by = 1)
legend_grid <- expand.grid(g, g)
legend_grid$colour <- palette16[c(13:16,
                                  9:12,
                                  5:8,
                                  1:4)]
ggplot(legend_grid, aes(x = Var1, y = Var2)) +
  geom_raster(fill = legend_grid$colour) +
  theme_minimal() +
  xlab("Correlation with IPO") +
  ylab("Temperature trend") +
  scale_x_continuous(breaks = seq(-1.5, 1.5, by = 1), labels = ipo_labels) +
  scale_y_continuous(breaks = seq(-1.5, 1.5, by = 1), labels = trend_labels) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
ggsave("figures/double_legend.pdf", width = 2.5, height = 2.5)
