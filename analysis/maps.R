request_all("^slopes_")

## x is an object as returned from slopes.R
slopes_ref <- function(x) {
  ## 1: sign. Hiatus
  ## 2: insign. Hiatus
  ## 3: sign. Anti-Hiatus
  ## 4: insign. Anti-Hiatus
  x <- x %>%
    unnest(sdif) %>%
    mutate(hiatus_mc = ifelse(slope_delta < 0 & delta_sig < 0.05, 1,
                              ifelse(slope_delta < 0 & delta_sig >=
                                       0.05, 2,
                                     ifelse(slope_delta > 0 & delta_sig < 0.05, 3, 4))),
           hiatus_chow = ifelse(slope_delta < 0 & chow < 0.05, 1,
                                ifelse(slope_delta < 0 & chow >= 0.05, 2,
                                       ifelse(slope_delta > 0 & chow <
                                                0.05, 3, 4)))) %>%
    mutate(hiatus_cons = ifelse(hiatus_mc == hiatus_chow, hiatus_mc, NA)) %>% 
    select(x = lon, y = lat, hiatus_mc, hiatus_chow, hiatus_cons) %>%
    as.data.frame
  x$hiatus_mc <- factor(x$hiatus_mc, levels = as.character(1:4))
  x$hiatus_chow <- factor(x$hiatus_chow, levels = as.character(1:4))
  x$hiatus_cons <- factor(x$hiatus_cons, levels = as.character(1:4))
  x
}

slopes_ref2 <- function(x) {
  ## 1: slowdown
  ## 2: acceleration
  x <- x %>%
    unnest(sdif) %>%
    mutate(change = ifelse(slope_delta < 0, 1, 2)) %>% 
    select(x = lon, y = lat, change) %>%
    as.data.frame
  x$change <- factor(x$change, levels = as.character(1:2))
  x
}

### Drawing the actual maps -----------------------------------------

draw_map <- function(s_gistemp, s_noaa, s_hadcrut, fname) {
  
  gistemp_R <- s_gistemp %>%
    slopes_ref %>%
    reraster

  ## different methods + consensus
  map_gistemp_R_mc <- tm_shape(gistemp_R) +
    tm_raster("hiatus_mc",
              palette = adaptive_palette(gistemp_R$hiatus_mc),
              legend.show = FALSE, auto.palette.mapping = FALSE,
              colorNA = "#e3e3e3") +
  map_layer
  
  map_gistemp_R_chow <- tm_shape(gistemp_R) +
    tm_raster("hiatus_chow",
              palette = adaptive_palette(gistemp_R$hiatus_chow),
              legend.show = FALSE, auto.palette.mapping = FALSE,
              colorNA = "#e3e3e3") +
    map_layer
  
  map_gistemp_R_cons <- tm_shape(gistemp_R) +
    tm_raster("hiatus_cons",
              palette = adaptive_palette(gistemp_R$hiatus_cons),
              legend.show = FALSE, auto.palette.mapping = FALSE,
              colorNA = "#e3e3e3") +
    map_layer

  hadcrut_R <- s_hadcrut %>%
    slopes_ref %>%
    reraster

  ## different methods + consensus
  map_hadcrut_R_mc <- tm_shape(hadcrut_R) +
    tm_raster("hiatus_mc",
              palette = adaptive_palette(hadcrut_R$hiatus_mc),
              legend.show = FALSE, auto.palette.mapping = FALSE,
              colorNA = "#e3e3e3") +
  map_layer
  
  map_hadcrut_R_chow <- tm_shape(hadcrut_R) +
    tm_raster("hiatus_chow",
              palette = adaptive_palette(hadcrut_R$hiatus_chow),
              legend.show = FALSE, auto.palette.mapping = FALSE,
              colorNA = "#e3e3e3") +
    map_layer
  
  map_hadcrut_R_cons <- tm_shape(hadcrut_R) +
    tm_raster("hiatus_cons",
              palette = adaptive_palette(hadcrut_R$hiatus_cons),
              legend.show = FALSE, auto.palette.mapping = FALSE,
              colorNA = "#e3e3e3") +
    map_layer

  noaa_R <- s_noaa %>%
    ## longitudes range from 0 to 360 and need to be fixed prior to
    ## remapping
    mutate(lon = ifelse(lon < 180, lon, lon - 360)) %>% 
    slopes_ref %>%
    reraster

  ## different methods + consensus
  map_noaa_R_mc <- tm_shape(noaa_R) +
    tm_raster("hiatus_mc",
              palette = adaptive_palette(noaa_R$hiatus_mc),
              legend.show = FALSE, auto.palette.mapping = FALSE,
              colorNA = "#e3e3e3") +
  map_layer
  
  map_noaa_R_chow <- tm_shape(noaa_R) +
    tm_raster("hiatus_chow",
              palette = adaptive_palette(noaa_R$hiatus_chow),
              legend.show = FALSE, auto.palette.mapping = FALSE,
              colorNA = "#e3e3e3") +
    map_layer
  
  map_noaa_R_cons <- tm_shape(noaa_R) +
    tm_raster("hiatus_cons",
              palette = adaptive_palette(noaa_R$hiatus_cons),
              legend.show = FALSE, auto.palette.mapping = FALSE,
              colorNA = "#e3e3e3") +
    map_layer

  fname <- paste0("figures/", fname)
  
  pdf(fname, width = 12, height = 9)
  tmap_arrange(map_gistemp_R_mc, map_noaa_R_mc, map_hadcrut_R_mc, 
               map_gistemp_R_chow, map_noaa_R_chow, map_hadcrut_R_chow, 
               map_gistemp_R_cons, map_noaa_R_cons, map_hadcrut_R_cons, 
               ncol = 3)
  dev.off()
}

### Not interpolated climate data -----------------------------------

## 1. unaltered p-values

## reference period
draw_map(slopes_gistemp_R,
         slopes_noaa_R,
         slopes_hadcrut_R,
         fname = "hiatus_R.pdf")

## 2000:2014
draw_map(slopes_gistemp_p2,
         slopes_noaa_p2,
         slopes_hadcrut_p2,
         fname = "hiatus_p2.pdf")

## 1999:2013
draw_map(slopes_gistemp_p3,
         slopes_noaa_p3,
         slopes_hadcrut_p3,
         fname = "hiatus_p3.pdf")

## 1998:2012
draw_map(slopes_gistemp_p4,
         slopes_noaa_p4,
         slopes_hadcrut_p4,
         fname = "hiatus_p4.pdf")

## 2. FDR-corrected p-values

## reference period
draw_map(slopes_gistemp_R_fdr,
         slopes_noaa_R_fdr,
         slopes_hadcrut_R_fdr,
         fname = "hiatus_R_fdr.pdf")

## 2000:2014
draw_map(slopes_gistemp_p2_fdr,
         slopes_noaa_p2_fdr,
         slopes_hadcrut_p2_fdr,
         fname = "hiatus_p2_fdr.pdf")

## 1999:2013
draw_map(slopes_gistemp_p3_fdr,
         slopes_noaa_p3_fdr,
         slopes_hadcrut_p3_fdr,
         fname = "hiatus_p3_fdr.pdf")

## 1998:2012
draw_map(slopes_gistemp_p4_fdr,
         slopes_noaa_p4_fdr,
         slopes_hadcrut_p4_fdr,
         fname = "hiatus_p4_fdr.pdf")

### Climate data w/ ENSO removed ---------------------------------------

## 1. unaltered p-values

## reference period
draw_map(slopes_meiresid_gistemp_R,
         slopes_meiresid_noaa_R,
         slopes_meiresid_hadcrut_R,
         fname = "hiatus_meiresid_R.pdf")

## 2000:2014
draw_map(slopes_meiresid_gistemp_p2,
         slopes_meiresid_noaa_p2,
         slopes_meiresid_hadcrut_p2,
         fname = "hiatus_meiresid_p2.pdf")

## 1999:2013
draw_map(slopes_meiresid_gistemp_p3,
         slopes_meiresid_noaa_p3,
         slopes_meiresid_hadcrut_p3,
         fname = "hiatus_meiresid_p3.pdf")

## 1998:2012
draw_map(slopes_meiresid_gistemp_p4,
         slopes_meiresid_noaa_p4,
         slopes_meiresid_hadcrut_p4,
         fname = "hiatus_meiresid_p4.pdf")

## 2. FDR-corrected p-values

## reference period
draw_map(slopes_meiresid_gistemp_R_fdr,
         slopes_meiresid_noaa_R_fdr,
         slopes_meiresid_hadcrut_R_fdr,
         fname = "hiatus_meiresid_R.pdf")

## 2000:2014
draw_map(slopes_meiresid_gistemp_p2_fdr,
         slopes_meiresid_noaa_p2_fdr,
         slopes_meiresid_hadcrut_p2_fdr,
         fname = "hiatus_meiresid_p2.pdf")

## 1999:2013
draw_map(slopes_meiresid_gistemp_p3_fdr,
         slopes_meiresid_noaa_p3_fdr,
         slopes_meiresid_hadcrut_p3_fdr,
         fname = "hiatus_meiresid_p3.pdf")

## 1998:2012
draw_map(slopes_meiresid_gistemp_p4_fdr,
         slopes_meiresid_noaa_p4_fdr,
         slopes_meiresid_hadcrut_p4_fdr,
         fname = "hiatus_meiresid_p4.pdf")

### Getting consensus style maps for pre/post interpolation ---------

## ! we do this based on the chow test results!

## GISTEMP
sr_gistemp_R <- slopes_ref(slopes_gistemp_R_fdr)
mei_cons_gistemp <- slopes_ref(slopes_meiresid_gistemp_R_fdr) %>%
  rename(int_hiatus_mc = hiatus_mc,
         int_hiatus_chow = hiatus_chow,
         int_hiatus_cons = hiatus_cons) %>%
  left_join(sr_gistemp_R) %>%
  ## 1: still hiatus -> dark blue
  ## 2: gone -> orange
  ## 3: new hiatus -> green
  ## 4: something else -> grey
  mutate(int_change_chow = ifelse(hiatus_chow == 1 & int_hiatus_chow == 1,
                                1,
                                ifelse(hiatus_chow == 1 & int_hiatus_chow !=
                                         1,
                                       2, ifelse(
                                       hiatus_chow != 1 & int_hiatus_chow ==
                                         1, 3, 4))))

mei_cons_gistemp %>%
  group_by(int_change_chow) %>%
  mutate(countfrac = 1/nrow(mei_cons_gistemp)) %>%
  summarise(countfrac = sum(countfrac))

sr_gistemp_p2 <- slopes_ref(slopes_gistemp_p2_fdr)
mei_cons_gistemp_p2 <- slopes_ref(slopes_meiresid_gistemp_p2_fdr) %>%
  rename(int_hiatus_mc = hiatus_mc,
         int_hiatus_chow = hiatus_chow,
         int_hiatus_cons = hiatus_cons) %>%
  left_join(sr_gistemp_p2) %>%
  ## 1: still hiatus -> dark blue
  ## 2: gone -> orange
  ## 3: new hiatus -> green
  ## 4: something else -> grey
  mutate(int_change_chow = ifelse(hiatus_chow == 1 & int_hiatus_chow == 1,
                                1,
                                ifelse(hiatus_chow == 1 & int_hiatus_chow !=
                                         1,
                                       2, ifelse(
                                       hiatus_chow != 1 & int_hiatus_chow ==
                                         1, 3, 4))))

mei_cons_gistemp_p2 %>%
  group_by(int_change_chow) %>%
  mutate(countfrac = 1/nrow(mei_cons_gistemp_p2)) %>%
  summarise(countfrac = sum(countfrac))

sr_gistemp_p3 <- slopes_ref(slopes_gistemp_p3_fdr)
mei_cons_gistemp_p3 <- slopes_ref(slopes_meiresid_gistemp_p3_fdr) %>%
  rename(int_hiatus_mc = hiatus_mc,
         int_hiatus_chow = hiatus_chow,
         int_hiatus_cons = hiatus_cons) %>%
  left_join(sr_gistemp_p3) %>%
  ## 1: still hiatus -> dark blue
  ## 2: gone -> orange
  ## 3: new hiatus -> green
  ## 4: something else -> grey
  mutate(int_change_chow = ifelse(hiatus_chow == 1 & int_hiatus_chow == 1,
                                1,
                                ifelse(hiatus_chow == 1 & int_hiatus_chow !=
                                         1,
                                       2, ifelse(
                                       hiatus_chow != 1 & int_hiatus_chow ==
                                         1, 3, 4))))

mei_cons_gistemp_p3 %>%
  group_by(int_change_chow) %>%
  mutate(countfrac = 1/nrow(mei_cons_gistemp_p3)) %>%
  summarise(countfrac = sum(countfrac))

sr_gistemp_p4 <- slopes_ref(slopes_gistemp_p4_fdr)
mei_cons_gistemp_p4 <- slopes_ref(slopes_meiresid_gistemp_p4_fdr) %>%
  rename(int_hiatus_mc = hiatus_mc,
         int_hiatus_chow = hiatus_chow,
         int_hiatus_cons = hiatus_cons) %>%
  left_join(sr_gistemp_p4) %>%
  ## 1: still hiatus -> dark blue
  ## 2: gone -> orange
  ## 3: new hiatus -> green
  ## 4: something else -> grey
  mutate(int_change_chow = ifelse(hiatus_chow == 1 & int_hiatus_chow == 1,
                                1,
                                ifelse(hiatus_chow == 1 & int_hiatus_chow !=
                                         1,
                                       2, ifelse(
                                       hiatus_chow != 1 & int_hiatus_chow ==
                                         1, 3, 4))))

mei_cons_gistemp_p4 %>%
  group_by(int_change_chow) %>%
  mutate(countfrac = 1/nrow(mei_cons_gistemp_p4)) %>%
  summarise(countfrac = sum(countfrac))

## draw maps
mei_cons_gistemp_rer_R <- mei_cons_gistemp %>%
  reraster

mei_cons_gistemp_rer_p2 <- mei_cons_gistemp_p2 %>%
  reraster

mei_cons_gistemp_rer_p3 <- mei_cons_gistemp_p3 %>%
  reraster

mei_cons_gistemp_rer_p4 <- mei_cons_gistemp_p4 %>%
  reraster

map_mei_cons_gistemp_R <- tm_shape(mei_cons_gistemp_rer_R) +
  tm_raster("int_change_chow",
            palette =
              adaptive_palette(mei_cons_gistemp_rer_R$int_change_chow,
                               fpalette),
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
map_layer

map_mei_cons_gistemp_p2 <- tm_shape(mei_cons_gistemp_rer_p2) +
  tm_raster("int_change_chow",
            palette =
              adaptive_palette(mei_cons_gistemp_rer_p2$int_change_chow,
                               fpalette),
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
map_layer

map_mei_cons_gistemp_p3 <- tm_shape(mei_cons_gistemp_rer_p3) +
  tm_raster("int_change_chow",
            palette =
              adaptive_palette(mei_cons_gistemp_rer_p3$int_change_chow,
                               fpalette),
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
map_layer

map_mei_cons_gistemp_p4 <- tm_shape(mei_cons_gistemp_rer_p4) +
  tm_raster("int_change_chow",
            palette =
              adaptive_palette(mei_cons_gistemp_rer_p4$int_change_chow,
                               fpalette),
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
map_layer

pdf("figures/mei_cons_mc_gistemp_fdr_sig.pdf", width = 8, height = 7)
tmap_arrange(map_mei_cons_gistemp_R,
             map_mei_cons_gistemp_p2,
             map_mei_cons_gistemp_p3,
             map_mei_cons_gistemp_p4, 
             nrow = 4)
dev.off()

## NOAA

sr_noaa_R <- slopes_ref(slopes_noaa_R_fdr)
mei_cons_noaa <- slopes_ref(slopes_meiresid_noaa_R_fdr) %>%
  rename(int_hiatus_mc = hiatus_mc,
         int_hiatus_chow = hiatus_chow,
         int_hiatus_cons = hiatus_cons) %>%
  left_join(sr_noaa_R) %>%
  ## 1: still hiatus -> dark blue
  ## 2: gone -> orange
  ## 3: new hiatus -> green
  ## 4: something else -> grey
  mutate(int_change_chow = ifelse(hiatus_chow == 1 & int_hiatus_chow == 1,
                                1,
                                ifelse(hiatus_chow == 1 & int_hiatus_chow !=
                                         1,
                                       2, ifelse(
                                       hiatus_chow != 1 & int_hiatus_chow ==
                                         1, 3, 4)))) %>% 
  mutate(x = ifelse(x < 180, x, x - 360))

sr_noaa_p2 <- slopes_ref(slopes_noaa_p2_fdr)
mei_cons_noaa_p2 <- slopes_ref(slopes_meiresid_noaa_p2_fdr) %>%
  rename(int_hiatus_mc = hiatus_mc,
         int_hiatus_chow = hiatus_chow,
         int_hiatus_cons = hiatus_cons) %>%
  left_join(sr_noaa_p2) %>%
  ## 1: still hiatus -> dark blue
  ## 2: gone -> orange
  ## 3: new hiatus -> green
  ## 4: something else -> grey
  mutate(int_change_chow = ifelse(hiatus_chow == 1 & int_hiatus_chow == 1,
                                1,
                                ifelse(hiatus_chow == 1 & int_hiatus_chow !=
                                         1,
                                       2, ifelse(
                                       hiatus_chow != 1 & int_hiatus_chow ==
                                         1, 3, 4)))) %>% 
  mutate(x = ifelse(x < 180, x, x - 360))

sr_noaa_p3 <- slopes_ref(slopes_noaa_p3_fdr)
mei_cons_noaa_p3 <- slopes_ref(slopes_meiresid_noaa_p3_fdr) %>%
  rename(int_hiatus_mc = hiatus_mc,
         int_hiatus_chow = hiatus_chow,
         int_hiatus_cons = hiatus_cons) %>%
  left_join(sr_noaa_p3) %>%
  ## 1: still hiatus -> dark blue
  ## 2: gone -> orange
  ## 3: new hiatus -> green
  ## 4: something else -> grey
  mutate(int_change_chow = ifelse(hiatus_chow == 1 & int_hiatus_chow == 1,
                                1,
                                ifelse(hiatus_chow == 1 & int_hiatus_chow !=
                                         1,
                                       2, ifelse(
                                       hiatus_chow != 1 & int_hiatus_chow ==
                                         1, 3, 4)))) %>% 
  mutate(x = ifelse(x < 180, x, x - 360))

sr_noaa_p4 <- slopes_ref(slopes_noaa_p4_fdr)
mei_cons_noaa_p4 <- slopes_ref(slopes_meiresid_noaa_p4_fdr) %>%
  rename(int_hiatus_mc = hiatus_mc,
         int_hiatus_chow = hiatus_chow,
         int_hiatus_cons = hiatus_cons) %>%
  left_join(sr_noaa_p4) %>%
  ## 1: still hiatus -> dark blue
  ## 2: gone -> orange
  ## 3: new hiatus -> green
  ## 4: something else -> grey
  mutate(int_change_chow = ifelse(hiatus_chow == 1 & int_hiatus_chow == 1,
                                1,
                                ifelse(hiatus_chow == 1 & int_hiatus_chow !=
                                         1,
                                       2, ifelse(
                                       hiatus_chow != 1 & int_hiatus_chow ==
                                         1, 3, 4)))) %>% 
  mutate(x = ifelse(x < 180, x, x - 360))

## draw maps
mei_cons_noaa_rer_R <- mei_cons_noaa %>%
  reraster

mei_cons_noaa_rer_p2 <- mei_cons_noaa_p2 %>%
  reraster

mei_cons_noaa_rer_p3 <- mei_cons_noaa_p3 %>%
  reraster

mei_cons_noaa_rer_p4 <- mei_cons_noaa_p4 %>%
  reraster

map_mei_cons_noaa_R <- tm_shape(mei_cons_noaa_rer_R) +
  tm_raster("int_change_chow",
            palette =
              adaptive_palette(mei_cons_noaa_rer_R$int_change_chow,
                               fpalette),
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
map_layer

map_mei_cons_noaa_p2 <- tm_shape(mei_cons_noaa_rer_p2) +
  tm_raster("int_change_chow",
            palette =
              adaptive_palette(mei_cons_noaa_rer_p2$int_change_chow,
                               fpalette),
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
map_layer

map_mei_cons_noaa_p3 <- tm_shape(mei_cons_noaa_rer_p3) +
  tm_raster("int_change_chow",
            palette =
              adaptive_palette(mei_cons_noaa_rer_p3$int_change_chow,
                               fpalette),
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
map_layer

map_mei_cons_noaa_p4 <- tm_shape(mei_cons_noaa_rer_p4) +
  tm_raster("int_change_chow",
            palette =
              adaptive_palette(mei_cons_noaa_rer_p4$int_change_chow,
                               fpalette),
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
map_layer

pdf("figures/mei_cons_mc_noaa_fdr_sig.pdf", width = 8, height = 7)
tmap_arrange(map_mei_cons_noaa_R,
             map_mei_cons_noaa_p2,
             map_mei_cons_noaa_p3,
             map_mei_cons_noaa_p4, 
             nrow = 4)
dev.off()

## HADCRUT

sr_hadcrut_R <- slopes_ref(slopes_hadcrut_R_fdr)
mei_cons_hadcrut <- slopes_ref(slopes_meiresid_hadcrut_R_fdr) %>%
  rename(int_hiatus_mc = hiatus_mc,
         int_hiatus_chow = hiatus_chow,
         int_hiatus_cons = hiatus_cons) %>%
  left_join(sr_hadcrut_R) %>%
  ## 1: still hiatus -> dark blue
  ## 2: gone -> orange
  ## 3: new hiatus -> green
  ## 4: something else -> grey
  mutate(int_change_chow = ifelse(hiatus_chow == 1 & int_hiatus_chow == 1,
                                1,
                                ifelse(hiatus_chow == 1 & int_hiatus_chow !=
                                         1,
                                       2, ifelse(
                                       hiatus_chow != 1 & int_hiatus_chow ==
                                         1, 3, 4))))

sr_hadcrut_p2 <- slopes_ref(slopes_hadcrut_p2_fdr)
mei_cons_hadcrut_p2 <- slopes_ref(slopes_meiresid_hadcrut_p2_fdr) %>%
  rename(int_hiatus_mc = hiatus_mc,
         int_hiatus_chow = hiatus_chow,
         int_hiatus_cons = hiatus_cons) %>%
  left_join(sr_hadcrut_p2) %>%
  ## 1: still hiatus -> dark blue
  ## 2: gone -> orange
  ## 3: new hiatus -> green
  ## 4: something else -> grey
  mutate(int_change_chow = ifelse(hiatus_chow == 1 & int_hiatus_chow == 1,
                                1,
                                ifelse(hiatus_chow == 1 & int_hiatus_chow !=
                                         1,
                                       2, ifelse(
                                       hiatus_chow != 1 & int_hiatus_chow ==
                                         1, 3, 4))))

sr_hadcrut_p3 <- slopes_ref(slopes_hadcrut_p3_fdr)
mei_cons_hadcrut_p3 <- slopes_ref(slopes_meiresid_hadcrut_p3_fdr) %>%
  rename(int_hiatus_mc = hiatus_mc,
         int_hiatus_chow = hiatus_chow,
         int_hiatus_cons = hiatus_cons) %>%
  left_join(sr_hadcrut_p3) %>%
  ## 1: still hiatus -> dark blue
  ## 2: gone -> orange
  ## 3: new hiatus -> green
  ## 4: something else -> grey
  mutate(int_change_chow = ifelse(hiatus_chow == 1 & int_hiatus_chow == 1,
                                1,
                                ifelse(hiatus_chow == 1 & int_hiatus_chow !=
                                         1,
                                       2, ifelse(
                                       hiatus_chow != 1 & int_hiatus_chow ==
                                         1, 3, 4))))

sr_hadcrut_p4 <- slopes_ref(slopes_hadcrut_p4_fdr)
mei_cons_hadcrut_p4 <- slopes_ref(slopes_meiresid_hadcrut_p4_fdr) %>%
  rename(int_hiatus_mc = hiatus_mc,
         int_hiatus_chow = hiatus_chow,
         int_hiatus_cons = hiatus_cons) %>%
  left_join(sr_hadcrut_p4) %>%
  ## 1: still hiatus -> dark blue
  ## 2: gone -> orange
  ## 3: new hiatus -> green
  ## 4: something else -> grey
  mutate(int_change_chow = ifelse(hiatus_chow == 1 & int_hiatus_chow == 1,
                                1,
                                ifelse(hiatus_chow == 1 & int_hiatus_chow !=
                                         1,
                                       2, ifelse(
                                       hiatus_chow != 1 & int_hiatus_chow ==
                                         1, 3, 4))))

## draw maps
mei_cons_hadcrut_rer_R <- mei_cons_hadcrut %>%
  reraster

mei_cons_hadcrut_rer_p2 <- mei_cons_hadcrut_p2 %>%
  reraster

mei_cons_hadcrut_rer_p3 <- mei_cons_hadcrut_p3 %>%
  reraster

mei_cons_hadcrut_rer_p4 <- mei_cons_hadcrut_p4 %>%
  reraster

map_mei_cons_hadcrut_R <- tm_shape(mei_cons_hadcrut_rer_R) +
  tm_raster("int_change_chow",
            palette =
              adaptive_palette(mei_cons_hadcrut_rer_R$int_change_chow,
                               fpalette),
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
map_layer

map_mei_cons_hadcrut_p2 <- tm_shape(mei_cons_hadcrut_rer_p2) +
  tm_raster("int_change_chow",
            palette =
              adaptive_palette(mei_cons_hadcrut_rer_p2$int_change_chow,
                               fpalette),
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
map_layer

map_mei_cons_hadcrut_p3 <- tm_shape(mei_cons_hadcrut_rer_p3) +
  tm_raster("int_change_chow",
            palette =
              adaptive_palette(mei_cons_hadcrut_rer_p3$int_change_chow,
                               fpalette),
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
map_layer

map_mei_cons_hadcrut_p4 <- tm_shape(mei_cons_hadcrut_rer_p4) +
  tm_raster("int_change_chow",
            palette =
              adaptive_palette(mei_cons_hadcrut_rer_p4$int_change_chow,
                               fpalette),
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
map_layer

pdf("figures/mei_cons_mc_hadcrut_fdr_sig.pdf", width = 8, height = 7)
tmap_arrange(map_mei_cons_hadcrut_R,
             map_mei_cons_hadcrut_p2,
             map_mei_cons_hadcrut_p3,
             map_mei_cons_hadcrut_p4, 
             nrow = 4)
dev.off()

### Consensus map for trend changes w/o looking for significance ----

## code as above with slight changes

## GISTEMP
sr_gistemp_R2 <- slopes_ref2(slopes_gistemp_R_fdr)
mei_cons_gistemp2 <- slopes_ref2(slopes_meiresid_gistemp_R_fdr) %>%
  rename(int_change = change) %>% 
  left_join(sr_gistemp_R2) %>% 
  ## 1: still hiatus -> dark blue
  ## 2: gone -> orange
  ## 3: new hiatus -> green
  ## 4: something else -> grey
  mutate(int_change = ifelse(change == 1 & int_change == 1, 1,
                             ifelse(change == 1 & int_change != 1, 2,
                                    ifelse(change != 1 & int_change == 1, 3, 4))))

mei_cons_gistemp2 %>%
  group_by(int_change) %>%
  mutate(countfrac = 1/nrow(mei_cons_gistemp)) %>%
  summarise(countfrac = sum(countfrac))

sr_gistemp_p22 <- slopes_ref2(slopes_gistemp_p2_fdr)
mei_cons_gistemp_p22 <- slopes_ref2(slopes_meiresid_gistemp_p2_fdr) %>%
  rename(int_change = change) %>% 
  left_join(sr_gistemp_p22) %>% 
  ## 1: still hiatus -> dark blue
  ## 2: gone -> orange
  ## 3: new hiatus -> green
  ## 4: something else -> grey
  mutate(int_change = ifelse(change == 1 & int_change == 1, 1,
                             ifelse(change == 1 & int_change != 1, 2,
                                    ifelse(change != 1 & int_change == 1, 3, 4))))

mei_cons_gistemp_p22 %>%
  group_by(int_change) %>%
  mutate(countfrac = 1/nrow(mei_cons_gistemp)) %>%
  summarise(countfrac = sum(countfrac))

sr_gistemp_p32 <- slopes_ref2(slopes_gistemp_p3_fdr)
mei_cons_gistemp_p32 <- slopes_ref2(slopes_meiresid_gistemp_p3_fdr) %>%
  rename(int_change = change) %>% 
  left_join(sr_gistemp_p32) %>% 
  ## 1: still hiatus -> dark blue
  ## 2: gone -> orange
  ## 3: new hiatus -> green
  ## 4: something else -> grey
  mutate(int_change = ifelse(change == 1 & int_change == 1, 1,
                             ifelse(change == 1 & int_change != 1, 2,
                                    ifelse(change != 1 & int_change == 1, 3, 4))))

mei_cons_gistemp_p32 %>%
  group_by(int_change) %>%
  mutate(countfrac = 1/nrow(mei_cons_gistemp)) %>%
  summarise(countfrac = sum(countfrac))

sr_gistemp_p42 <- slopes_ref2(slopes_gistemp_p4_fdr)
mei_cons_gistemp_p42 <- slopes_ref2(slopes_meiresid_gistemp_p4_fdr) %>%
  rename(int_change = change) %>% 
  left_join(sr_gistemp_p42) %>% 
  ## 1: still hiatus -> dark blue
  ## 2: gone -> orange
  ## 3: new hiatus -> green
  ## 4: something else -> grey
  mutate(int_change = ifelse(change == 1 & int_change == 1, 1,
                             ifelse(change == 1 & int_change != 1, 2,
                                    ifelse(change != 1 & int_change == 1, 3, 4))))

mei_cons_gistemp_p42 %>%
  group_by(int_change) %>%
  mutate(countfrac = 1/nrow(mei_cons_gistemp)) %>%
  summarise(countfrac = sum(countfrac))

## draw maps
mei_cons_gistemp_rer_R2 <- mei_cons_gistemp2 %>%
  reraster

mei_cons_gistemp_rer_p22 <- mei_cons_gistemp_p22 %>%
  reraster

mei_cons_gistemp_rer_p32 <- mei_cons_gistemp_p32 %>%
  reraster

mei_cons_gistemp_rer_p42 <- mei_cons_gistemp_p42 %>%
  reraster

map_mei_cons_gistemp_R2 <- tm_shape(mei_cons_gistemp_rer_R2) +
  tm_raster("int_change",
            palette =
              adaptive_palette(mei_cons_gistemp_rer_R2$int_change,
                               fpalette),
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
map_layer

map_mei_cons_gistemp_p22 <- tm_shape(mei_cons_gistemp_rer_p22) +
  tm_raster("int_change",
            palette =
              adaptive_palette(mei_cons_gistemp_rer_p22$int_change,
                               fpalette),
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
map_layer

map_mei_cons_gistemp_p32 <- tm_shape(mei_cons_gistemp_rer_p32) +
  tm_raster("int_change",
            palette =
              adaptive_palette(mei_cons_gistemp_rer_p32$int_change,
                               fpalette),
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
map_layer

map_mei_cons_gistemp_p42 <- tm_shape(mei_cons_gistemp_rer_p42) +
  tm_raster("int_change",
            palette =
              adaptive_palette(mei_cons_gistemp_rer_p42$int_change,
                               fpalette),
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
map_layer

pdf("figures/mei_cons_mc_gistemp_fdr_all.pdf", width = 8, height = 7)
tmap_arrange(map_mei_cons_gistemp_R2,
             map_mei_cons_gistemp_p22,
             map_mei_cons_gistemp_p32,
             map_mei_cons_gistemp_p42, 
             nrow = 4)
dev.off()

## NOAA
sr_noaa_R2 <- slopes_ref2(slopes_noaa_R_fdr)
mei_cons_noaa2 <- slopes_ref2(slopes_meiresid_noaa_R_fdr) %>%
  rename(int_change = change) %>% 
  left_join(sr_noaa_R2) %>% 
  ## 1: still hiatus -> dark blue
  ## 2: gone -> orange
  ## 3: new hiatus -> green
  ## 4: something else -> grey
  mutate(int_change = ifelse(change == 1 & int_change == 1, 1,
                             ifelse(change == 1 & int_change != 1, 2,
                                    ifelse(change != 1 & int_change == 1, 3, 4)))) %>% 
  mutate(x = ifelse(x < 180, x, x - 360))

mei_cons_noaa2 %>%
  group_by(int_change) %>%
  mutate(countfrac = 1/nrow(mei_cons_noaa)) %>%
  summarise(countfrac = sum(countfrac))

sr_noaa_p22 <- slopes_ref2(slopes_noaa_p2_fdr)
mei_cons_noaa_p22 <- slopes_ref2(slopes_meiresid_noaa_p2_fdr) %>%
  rename(int_change = change) %>% 
  left_join(sr_noaa_p22) %>% 
  ## 1: still hiatus -> dark blue
  ## 2: gone -> orange
  ## 3: new hiatus -> green
  ## 4: something else -> grey
  mutate(int_change = ifelse(change == 1 & int_change == 1, 1,
                             ifelse(change == 1 & int_change != 1, 2,
                                    ifelse(change != 1 & int_change == 1, 3, 4)))) %>% 
  mutate(x = ifelse(x < 180, x, x - 360))

mei_cons_noaa_p22 %>%
  group_by(int_change) %>%
  mutate(countfrac = 1/nrow(mei_cons_noaa)) %>%
  summarise(countfrac = sum(countfrac))

sr_noaa_p32 <- slopes_ref2(slopes_noaa_p3_fdr)
mei_cons_noaa_p32 <- slopes_ref2(slopes_meiresid_noaa_p3_fdr) %>%
  rename(int_change = change) %>% 
  left_join(sr_noaa_p32) %>% 
  ## 1: still hiatus -> dark blue
  ## 2: gone -> orange
  ## 3: new hiatus -> green
  ## 4: something else -> grey
  mutate(int_change = ifelse(change == 1 & int_change == 1, 1,
                             ifelse(change == 1 & int_change != 1, 2,
                                    ifelse(change != 1 & int_change == 1, 3, 4)))) %>% 
  mutate(x = ifelse(x < 180, x, x - 360))

mei_cons_noaa_p32 %>%
  group_by(int_change) %>%
  mutate(countfrac = 1/nrow(mei_cons_noaa)) %>%
  summarise(countfrac = sum(countfrac))

sr_noaa_p42 <- slopes_ref2(slopes_noaa_p4_fdr)
mei_cons_noaa_p42 <- slopes_ref2(slopes_meiresid_noaa_p4_fdr) %>%
  rename(int_change = change) %>% 
  left_join(sr_noaa_p42) %>% 
  ## 1: still hiatus -> dark blue
  ## 2: gone -> orange
  ## 3: new hiatus -> green
  ## 4: something else -> grey
  mutate(int_change = ifelse(change == 1 & int_change == 1, 1,
                             ifelse(change == 1 & int_change != 1, 2,
                                    ifelse(change != 1 & int_change == 1, 3, 4)))) %>% 
  mutate(x = ifelse(x < 180, x, x - 360))

mei_cons_noaa_p42 %>%
  group_by(int_change) %>%
  mutate(countfrac = 1/nrow(mei_cons_noaa)) %>%
  summarise(countfrac = sum(countfrac))

## draw maps
mei_cons_noaa_rer_R2 <- mei_cons_noaa2 %>%
  reraster

mei_cons_noaa_rer_p22 <- mei_cons_noaa_p22 %>%
  reraster

mei_cons_noaa_rer_p32 <- mei_cons_noaa_p32 %>%
  reraster

mei_cons_noaa_rer_p42 <- mei_cons_noaa_p42 %>%
  reraster

map_mei_cons_noaa_R2 <- tm_shape(mei_cons_noaa_rer_R2) +
  tm_raster("int_change",
            palette =
              adaptive_palette(mei_cons_noaa_rer_R2$int_change,
                               fpalette),
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
map_layer

map_mei_cons_noaa_p22 <- tm_shape(mei_cons_noaa_rer_p22) +
  tm_raster("int_change",
            palette =
              adaptive_palette(mei_cons_noaa_rer_p22$int_change,
                               fpalette),
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
map_layer

map_mei_cons_noaa_p32 <- tm_shape(mei_cons_noaa_rer_p32) +
  tm_raster("int_change",
            palette =
              adaptive_palette(mei_cons_noaa_rer_p32$int_change,
                               fpalette),
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
map_layer

map_mei_cons_noaa_p42 <- tm_shape(mei_cons_noaa_rer_p42) +
  tm_raster("int_change",
            palette =
              adaptive_palette(mei_cons_noaa_rer_p42$int_change,
                               fpalette),
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
map_layer

pdf("figures/mei_cons_mc_noaa_fdr_all.pdf", width = 8, height = 7)
tmap_arrange(map_mei_cons_noaa_R2,
             map_mei_cons_noaa_p22,
             map_mei_cons_noaa_p32,
             map_mei_cons_noaa_p42, 
             nrow = 4)
dev.off()

## HADCRUT
sr_hadcrut_R2 <- slopes_ref2(slopes_hadcrut_R_fdr)
mei_cons_hadcrut2 <- slopes_ref2(slopes_meiresid_hadcrut_R_fdr) %>%
  rename(int_change = change) %>% 
  left_join(sr_hadcrut_R2) %>% 
  ## 1: still hiatus -> dark blue
  ## 2: gone -> orange
  ## 3: new hiatus -> green
  ## 4: something else -> grey
  mutate(int_change = ifelse(change == 1 & int_change == 1, 1,
                             ifelse(change == 1 & int_change != 1, 2,
                                    ifelse(change != 1 & int_change == 1, 3, 4))))

mei_cons_hadcrut2 %>%
  group_by(int_change) %>%
  mutate(countfrac = 1/nrow(mei_cons_hadcrut)) %>%
  summarise(countfrac = sum(countfrac))

sr_hadcrut_p22 <- slopes_ref2(slopes_hadcrut_p2_fdr)
mei_cons_hadcrut_p22 <- slopes_ref2(slopes_meiresid_hadcrut_p2_fdr) %>%
  rename(int_change = change) %>% 
  left_join(sr_hadcrut_p22) %>% 
  ## 1: still hiatus -> dark blue
  ## 2: gone -> orange
  ## 3: new hiatus -> green
  ## 4: something else -> grey
  mutate(int_change = ifelse(change == 1 & int_change == 1, 1,
                             ifelse(change == 1 & int_change != 1, 2,
                                    ifelse(change != 1 & int_change == 1, 3, 4))))

mei_cons_hadcrut_p22 %>%
  group_by(int_change) %>%
  mutate(countfrac = 1/nrow(mei_cons_hadcrut)) %>%
  summarise(countfrac = sum(countfrac))

sr_hadcrut_p32 <- slopes_ref2(slopes_hadcrut_p3_fdr)
mei_cons_hadcrut_p32 <- slopes_ref2(slopes_meiresid_hadcrut_p3_fdr) %>%
  rename(int_change = change) %>% 
  left_join(sr_hadcrut_p32) %>% 
  ## 1: still hiatus -> dark blue
  ## 2: gone -> orange
  ## 3: new hiatus -> green
  ## 4: something else -> grey
  mutate(int_change = ifelse(change == 1 & int_change == 1, 1,
                             ifelse(change == 1 & int_change != 1, 2,
                                    ifelse(change != 1 & int_change == 1, 3, 4))))

mei_cons_hadcrut_p32 %>%
  group_by(int_change) %>%
  mutate(countfrac = 1/nrow(mei_cons_hadcrut)) %>%
  summarise(countfrac = sum(countfrac))

sr_hadcrut_p42 <- slopes_ref2(slopes_hadcrut_p4_fdr)
mei_cons_hadcrut_p42 <- slopes_ref2(slopes_meiresid_hadcrut_p4_fdr) %>%
  rename(int_change = change) %>% 
  left_join(sr_hadcrut_p42) %>% 
  ## 1: still hiatus -> dark blue
  ## 2: gone -> orange
  ## 3: new hiatus -> green
  ## 4: something else -> grey
  mutate(int_change = ifelse(change == 1 & int_change == 1, 1,
                             ifelse(change == 1 & int_change != 1, 2,
                                    ifelse(change != 1 & int_change == 1, 3, 4))))

mei_cons_hadcrut_p42 %>%
  group_by(int_change) %>%
  mutate(countfrac = 1/nrow(mei_cons_hadcrut)) %>%
  summarise(countfrac = sum(countfrac))

## draw maps
mei_cons_hadcrut_rer_R2 <- mei_cons_hadcrut2 %>%
  reraster

mei_cons_hadcrut_rer_p22 <- mei_cons_hadcrut_p22 %>%
  reraster

mei_cons_hadcrut_rer_p32 <- mei_cons_hadcrut_p32 %>%
  reraster

mei_cons_hadcrut_rer_p42 <- mei_cons_hadcrut_p42 %>%
  reraster

map_mei_cons_hadcrut_R2 <- tm_shape(mei_cons_hadcrut_rer_R2) +
  tm_raster("int_change",
            palette =
              adaptive_palette(mei_cons_hadcrut_rer_R2$int_change,
                               fpalette),
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
map_layer

map_mei_cons_hadcrut_p22 <- tm_shape(mei_cons_hadcrut_rer_p22) +
  tm_raster("int_change",
            palette =
              adaptive_palette(mei_cons_hadcrut_rer_p22$int_change,
                               fpalette),
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
map_layer

map_mei_cons_hadcrut_p32 <- tm_shape(mei_cons_hadcrut_rer_p32) +
  tm_raster("int_change",
            palette =
              adaptive_palette(mei_cons_hadcrut_rer_p32$int_change,
                               fpalette),
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
map_layer

map_mei_cons_hadcrut_p42 <- tm_shape(mei_cons_hadcrut_rer_p42) +
  tm_raster("int_change",
            palette =
              adaptive_palette(mei_cons_hadcrut_rer_p42$int_change,
                               fpalette),
            legend.show = FALSE, auto.palette.mapping = FALSE,
            colorNA = "#e3e3e3") +
map_layer

pdf("figures/mei_cons_mc_hadcrut_fdr_all.pdf", width = 8, height = 7)
tmap_arrange(map_mei_cons_hadcrut_R2,
             map_mei_cons_hadcrut_p22,
             map_mei_cons_hadcrut_p32,
             map_mei_cons_hadcrut_p42, 
             nrow = 4)
dev.off()


### Use information for % change ------------------------------------

## 1. GISTEMP

original_hiatus <- nrow(mei_cons_gistemp %>% filter(hiatus_chow == 1))
mei_cons_gistemp %>%
  group_by(int_change_chow) %>%
  summarise(perc = n() * 100 / original_hiatus) %>%
  filter(int_change_chow %in% c(2, 3))  # 2: gone; 3: new

original_hiatus <- nrow(mei_cons_gistemp_p2 %>% filter(hiatus_chow == 1))
mei_cons_gistemp_p2 %>%
  group_by(int_change_chow) %>%
  summarise(perc = n() * 100 / original_hiatus) %>%
  filter(int_change_chow %in% c(2, 3))  # 2: gone; 3: new

original_hiatus <- nrow(mei_cons_gistemp_p3 %>% filter(hiatus_chow == 1))
mei_cons_gistemp_p3 %>%
  group_by(int_change_chow) %>%
  summarise(perc = n() * 100 /original_hiatus) %>%
  filter(int_change_chow %in% c(2, 3))  # 2: gone; 3: new

original_hiatus <- nrow(mei_cons_gistemp_p4 %>% filter(hiatus_chow == 1))
mei_cons_gistemp_p4 %>%
  group_by(int_change_chow) %>%
  summarise(perc = n() * 100 /original_hiatus) %>%
  filter(int_change_chow %in% c(2, 3))  # 2: gone; 3: new
  
## 2. NOAA

original_hiatus <- nrow(mei_cons_noaa %>% filter(hiatus_chow == 1))
mei_cons_noaa %>%
  group_by(int_change_chow) %>%
  summarise(perc = n() * 100 /original_hiatus) %>%
  filter(int_change_chow %in% c(2, 3))  # 2: gone; 3: new

original_hiatus <- nrow(mei_cons_noaa_p2 %>% filter(hiatus_chow == 1))
mei_cons_noaa_p2 %>%
  group_by(int_change_chow) %>%
  summarise(perc = n() * 100 /original_hiatus) %>%
  filter(int_change_chow %in% c(2, 3))  # 2: gone; 3: new

original_hiatus <- nrow(mei_cons_noaa_p3 %>% filter(hiatus_chow == 1))
mei_cons_noaa_p3 %>%
  group_by(int_change_chow) %>%
  summarise(perc = n() * 100 /original_hiatus) %>%
  filter(int_change_chow %in% c(2, 3))  # 2: gone; 3: new

original_hiatus <- nrow(mei_cons_noaa_p4 %>% filter(hiatus_chow == 1))
mei_cons_noaa_p4 %>%
  group_by(int_change_chow) %>%
  summarise(perc = n() * 100 /original_hiatus) %>%
  filter(int_change_chow %in% c(2, 3))  # 2: gone; 3: new

## 3. HADCRUT

original_hiatus <- nrow(mei_cons_hadcrut %>% filter(hiatus_chow == 1))
mei_cons_hadcrut %>%
  group_by(int_change_chow) %>%
  summarise(perc = n() * 100 /original_hiatus) %>%
  filter(int_change_chow %in% c(2, 3))  # 2: gone; 3: new

original_hiatus <- nrow(mei_cons_hadcrut_p2 %>% filter(hiatus_chow == 1))
mei_cons_hadcrut_p2 %>%
  group_by(int_change_chow) %>%
  summarise(perc = n() * 100 /original_hiatus) %>%
  filter(int_change_chow %in% c(2, 3))  # 2: gone; 3: new

original_hiatus <- nrow(mei_cons_hadcrut_p3 %>% filter(hiatus_chow == 1))
mei_cons_hadcrut_p3 %>%
  group_by(int_change_chow) %>%
  summarise(perc = n() * 100 /original_hiatus) %>%
  filter(int_change_chow %in% c(2, 3))  # 2: gone; 3: new

original_hiatus <- nrow(mei_cons_hadcrut_p4 %>% filter(hiatus_chow == 1))
mei_cons_hadcrut_p4 %>%
  group_by(int_change_chow) %>%
  summarise(perc = n() * 100 /original_hiatus) %>%
  filter(int_change_chow %in% c(2, 3))  # 2: gone; 3: new

## relative to number of grid cells

## 1. GISTEMP

original_hiatus <- nrow(mei_cons_gistemp)
mei_cons_gistemp %>%
  group_by(int_change_chow) %>%
  summarise(perc = n() * 100 / original_hiatus) %>%
  filter(int_change_chow %in% c(2, 3))  # 2: gone; 3: new

original_hiatus <- nrow(mei_cons_gistemp_p2)
mei_cons_gistemp_p2 %>%
  group_by(int_change_chow) %>%
  summarise(perc = n() * 100 / original_hiatus) %>%
  filter(int_change_chow %in% c(2, 3))  # 2: gone; 3: new

original_hiatus <- nrow(mei_cons_gistemp_p3)
mei_cons_gistemp_p3 %>%
  group_by(int_change_chow) %>%
  summarise(perc = n() * 100 /original_hiatus) %>%
  filter(int_change_chow %in% c(2, 3))  # 2: gone; 3: new

original_hiatus <- nrow(mei_cons_gistemp_p4)
mei_cons_gistemp_p4 %>%
  group_by(int_change_chow) %>%
  summarise(perc = n() * 100 /original_hiatus) %>%
  filter(int_change_chow %in% c(2, 3))  # 2: gone; 3: new
  
## 2. NOAA

original_hiatus <- nrow(mei_cons_noaa)
mei_cons_noaa %>%
  group_by(int_change_chow) %>%
  summarise(perc = n() * 100 /original_hiatus) %>%
  filter(int_change_chow %in% c(2, 3))  # 2: gone; 3: new

original_hiatus <- nrow(mei_cons_noaa_p2)
mei_cons_noaa_p2 %>%
  group_by(int_change_chow) %>%
  summarise(perc = n() * 100 /original_hiatus) %>%
  filter(int_change_chow %in% c(2, 3))  # 2: gone; 3: new

original_hiatus <- nrow(mei_cons_noaa_p3)
mei_cons_noaa_p3 %>%
  group_by(int_change_chow) %>%
  summarise(perc = n() * 100 /original_hiatus) %>%
  filter(int_change_chow %in% c(2, 3))  # 2: gone; 3: new

original_hiatus <- nrow(mei_cons_noaa_p4)
mei_cons_noaa_p4 %>%
  group_by(int_change_chow) %>%
  summarise(perc = n() * 100 /original_hiatus) %>%
  filter(int_change_chow %in% c(2, 3))  # 2: gone; 3: new

## 3. HADCRUT

original_hiatus <- nrow(mei_cons_hadcrut)
mei_cons_hadcrut %>%
  group_by(int_change_chow) %>%
  summarise(perc = n() * 100 /original_hiatus) %>%
  filter(int_change_chow %in% c(2, 3))  # 2: gone; 3: new

original_hiatus <- nrow(mei_cons_hadcrut_p2)
mei_cons_hadcrut_p2 %>%
  group_by(int_change_chow) %>%
  summarise(perc = n() * 100 /original_hiatus) %>%
  filter(int_change_chow %in% c(2, 3))  # 2: gone; 3: new

original_hiatus <- nrow(mei_cons_hadcrut_p3)
mei_cons_hadcrut_p3 %>%
  group_by(int_change_chow) %>%
  summarise(perc = n() * 100 /original_hiatus) %>%
  filter(int_change_chow %in% c(2, 3))  # 2: gone; 3: new

original_hiatus <- nrow(mei_cons_hadcrut_p4)
mei_cons_hadcrut_p4 %>%
  group_by(int_change_chow) %>%
  summarise(perc = n() * 100 /original_hiatus) %>%
  filter(int_change_chow %in% c(2, 3))  # 2: gone; 3: new

### Same for all slowdowns ------------------------------------------

## gistemp

original_hiatus_all <- nrow(mei_cons_gistemp2)
mei_cons_gistemp2 %>%
  group_by(int_change) %>%
  summarise(perc = n() * 100 /original_hiatus_all) %>%
  filter(int_change %in% c(2, 3))  # 2: gone; 3: new

original_hiatus_all <- nrow(mei_cons_gistemp_p22)
mei_cons_gistemp_p22 %>%
  group_by(int_change) %>%
  summarise(perc = n() * 100 /original_hiatus_all) %>%
  filter(int_change %in% c(2, 3))  # 2: gone; 3: new

original_hiatus_all <- nrow(mei_cons_gistemp_p32)
mei_cons_gistemp_p32 %>%
  group_by(int_change) %>%
  summarise(perc = n() * 100 /original_hiatus_all) %>%
  filter(int_change %in% c(2, 3))  # 2: gone; 3: new

original_hiatus_all <- nrow(mei_cons_gistemp_p42)
mei_cons_gistemp_p42 %>%
  group_by(int_change) %>%
  summarise(perc = n() * 100 /original_hiatus_all) %>%
  filter(int_change %in% c(2, 3))  # 2: gone; 3: new

## noaa

original_hiatus_all <- nrow(mei_cons_noaa2)
mei_cons_noaa2 %>%
  group_by(int_change) %>%
  summarise(perc = n() * 100 /original_hiatus_all) %>%
  filter(int_change %in% c(2, 3))  # 2: gone; 3: new

original_hiatus_all <- nrow(mei_cons_noaa_p22)
mei_cons_noaa_p22 %>%
  group_by(int_change) %>%
  summarise(perc = n() * 100 /original_hiatus_all) %>%
  filter(int_change %in% c(2, 3))  # 2: gone; 3: new

original_hiatus_all <- nrow(mei_cons_noaa_p32)
mei_cons_noaa_p32 %>%
  group_by(int_change) %>%
  summarise(perc = n() * 100 /original_hiatus_all) %>%
  filter(int_change %in% c(2, 3))  # 2: gone; 3: new

original_hiatus_all <- nrow(mei_cons_noaa_p42)
mei_cons_noaa_p42 %>%
  group_by(int_change) %>%
  summarise(perc = n() * 100 /original_hiatus_all) %>%
  filter(int_change %in% c(2, 3))  # 2: gone; 3: new

## hadcrut

original_hiatus_all <- nrow(mei_cons_hadcrut2)
mei_cons_hadcrut2 %>%
  group_by(int_change) %>%
  summarise(perc = n() * 100 /original_hiatus_all) %>%
  filter(int_change %in% c(2, 3))  # 2: gone; 3: new

original_hiatus_all <- nrow(mei_cons_hadcrut_p22)
mei_cons_hadcrut_p22 %>%
  group_by(int_change) %>%
  summarise(perc = n() * 100 /original_hiatus_all) %>%
  filter(int_change %in% c(2, 3))  # 2: gone; 3: new

original_hiatus_all <- nrow(mei_cons_hadcrut_p32)
mei_cons_hadcrut_p32 %>%
  group_by(int_change) %>%
  summarise(perc = n() * 100 /original_hiatus_all) %>%
  filter(int_change %in% c(2, 3))  # 2: gone; 3: new

original_hiatus_all <- nrow(mei_cons_hadcrut_p42)
mei_cons_hadcrut_p42 %>%
  group_by(int_change) %>%
  summarise(perc = n() * 100 /original_hiatus_all) %>%
  filter(int_change %in% c(2, 3))  # 2: gone; 3: new
