## plot a few regions for example

## Hiatus:
## Azores:    (-28.25, 38.75)
## Carauari:  (-66.75, -4.75)
## Perth:     (115.75, -31.75)

## alternative:
## North Atlantic: (-39.75, 42.75)
## Wichita:        (-97.25, 37.75)
## Yannarie:       (115.25, -23.25)

request_all("slopes_(gistemp|noaa|hadcrut)")

example_regions <- function(cut = 2001, data_period = 1972:2014, fname) {
  
  ## YANNARIE

  slopes_gistemp_R %>%
    filter(lon == 115, lat == -23) %>%
    unnest(data) %>%
    filter(year %in% data_period) %>%
    mutate(period = ifelse(year < cut, "BP", "SP")) %>%
    mutate(dataset = "GISTEMP",
           datapoint = "Yannarie") -> perth_gistemp_p1

  slopes_noaa_R %>%
    mutate(lon = ifelse(lon < 180, lon, lon - 360)) %>% 
    filter(lon == 117.5, lat == -22.5) %>%
    unnest(data) %>%
    filter(year %in% data_period) %>%
    mutate(period = ifelse(year < cut, "BP", "SP")) %>%
    mutate(dataset = "NOAAGlobalTemp",
           datapoint = "Yannarie") -> perth_noaa_p1

  slopes_hadcrut_R %>%
    filter(lon == 117.5, lat == -22.5) %>%
    unnest(data) %>%
    filter(year %in% data_period) %>%
    mutate(period = ifelse(year < cut, "BP", "SP")) %>%
    mutate(dataset = "HADCRUT",
           datapoint = "Yannarie") -> perth_hadcrut_p1

  ## NORTH ATLANTIC

  slopes_gistemp_R %>%
    filter(lon == -39, lat == 43) %>%
    unnest(data) %>%
    filter(year %in% data_period) %>%
    mutate(period = ifelse(year < cut, "BP", "SP")) %>%
    mutate(dataset = "GISTEMP",
           datapoint = "North Atlantic") -> azores_gistemp_p1

  slopes_noaa_R %>%
    mutate(lon = ifelse(lon < 180, lon, lon - 360)) %>% 
    filter(lon == -37.5, lat == 42.5) %>%
    unnest(data) %>%
    filter(year %in% data_period) %>%
    mutate(period = ifelse(year < cut, "BP", "SP")) %>%
    mutate(dataset = "NOAAGlobalTemp",
           datapoint = "North Atlantic") -> azores_noaa_p1

  slopes_hadcrut_R %>%
    filter(lon == -37.5, lat == 42.5) %>%
    unnest(data) %>%
    filter(year %in% data_period) %>%
    mutate(period = ifelse(year < cut, "BP", "SP")) %>%
    mutate(dataset = "HADCRUT",
           datapoint = "North Atlantic") -> azores_hadcrut_p1

  ## TRINIDAD

  slopes_gistemp_R %>%
    filter(lon == -97, lat == 37) %>%
    unnest(data) %>%
    filter(year %in% data_period) %>%
    mutate(period = ifelse(year < cut, "BP", "SP")) %>%
    mutate(dataset = "GISTEMP",
           datapoint = "Wichita") -> carauari_gistemp_p1

  slopes_noaa_R %>%
    mutate(lon = ifelse(lon < 180, lon, lon - 360)) %>% 
    filter(lon == -97.5, lat == 37.5) %>%
    unnest(data) %>%
    filter(year %in% data_period) %>%
    mutate(period = ifelse(year < cut, "BP", "SP")) %>%
    mutate(dataset = "NOAAGlobalTemp",
           datapoint = "Wichita") -> carauari_noaa_p1

  slopes_hadcrut_R %>%
    filter(lon == -97.5, lat == 37.5) %>%
    unnest(data) %>%
    filter(year %in% data_period) %>%
    mutate(period = ifelse(year < cut, "BP", "SP")) %>%
    mutate(dataset = "HADCRUT",
           datapoint = "Wichita") -> carauari_hadcrut_p1

  ## MERGE

  rbind(
    perth_gistemp_p1,
    perth_noaa_p1,
    perth_hadcrut_p1,
    azores_gistemp_p1,
    azores_noaa_p1,
    azores_hadcrut_p1,
    carauari_gistemp_p1,
    carauari_noaa_p1,
    carauari_hadcrut_p1
  ) %>%
    ggplot(aes(x = year, y = temp, colour = dataset)) +
    geom_line(alpha = 0.5) +
    geom_smooth(aes(linetype = period), method = "lm", se = FALSE) +
    scale_linetype_manual(values = c(1, 1), guide = FALSE) +
    scale_color_discrete(name = "Dataset") +
    facet_grid(datapoint ~ .) +
    theme_bw() +
    ylab("Temperature anomaly (°C)") +
    xlab("Year")

  ggsave(paste0("figures/", fname), width = 8, height = 4,
         dpi = 600, compression = "lzw")

}

example_regions(cut = 2001, data_period = 1972:2014,
                fname = "example_regions_p1.png")
example_regions(cut = 2001, data_period = 1972:2014,
                fname = "example_regions_p1_submission.tiff")
example_regions(cut = 2000, data_period = 1972:2014,
                fname = "example_regions_p2.png")
example_regions(cut = 1999, data_period = 1972:2013,
                fname = "example_regions_p3.png")
example_regions(cut = 1998, data_period = 1972:2012,
                fname = "example_regions_p4.png")
