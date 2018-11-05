## model local SAT as residuals from regression against MEI series

mei <- read.table("data/mei.txt", header = TRUE)
mei <- mei %>% gather(month, mei, -YEAR) %>%
  rename(year = YEAR) %>% 
  group_by(year) %>%
  summarise(mei = mean(mei))

## x is a data frame holding the observations for one grid cell
mei_residuals <- function(x) {
  .x <- inner_join(x, mei)
  m <- lm(temp ~ mei, data = .x)
  yrs_sub <- x$year %in% .x$year
  x$temp[yrs_sub] <- resid(m) + coef(m)[1]
  x
}

## model local SAT as residuals from regression against MEI series;
## but only replace values for 1997 and 1998 -> this is more gradual
## than linear interpolation, but not associated with the trouble of
## removing all variability in the ENSO regions

## x is a data frame holding the observations for one grid cell
mei_residuals_9798 <- function(x) {
  en_yrs <- c(1997, 1998)
  .x <- inner_join(x, mei)
  m <- lm(temp ~ mei, data = .x)
  yrs_sub <- x$year %in% .x$year
  mod <- data.frame(
    year = x$year[yrs_sub],
    temp = resid(m) + coef(m)[1]
  )
  x$temp[x$year %in% en_yrs] <- mod$temp[mod$year %in% en_yrs]
  x
}

## register function for removing MEI influence based on global
## setting
remove_enso <- switch(REMOVE_ENSO,
                     full = mei_residuals,
                     en9798 = mei_residuals_9798)

nothing <- function(x) {
  x
}
