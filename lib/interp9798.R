# interpolate climate data for 97/98 based on long-term trend

interp9798 <- function(x) {
  .x <- x
  p97 <- which(x$year == 1997)
  pred <- lm(temp ~ year, data = x) %>% predict
  .x$temp[p97] <- pred[p97]
  .x$temp[p97 + 1] <- pred[p97 + 1]
  .x
}

nothing <- function(x) {
  x
}
