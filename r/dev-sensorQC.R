# sensorQC Package
# https://github.com/USGS-R/sensorQC

library(sensorQC)

file <- system.file('extdata', 'test_data.txt', package = 'sensorQC')
sensor <- read(file, format="wide_burst", date.format="%m/%d/%Y %H:%M")

sensor_flag1 <- flag(sensor, 'x == 999999', 'persist(x) > 3', 'is.na(x)')
sensor_flag1$flags

sensor_w1 <- window(clean(sensor_flag1, which = 1), type = "auto")
sensor_w1_flag <- flag(sensor_w1, 'x == 999999', 'persist(x) > 3', 'MAD(x,w) > 3', 'MAD(x) > 3')
sensor_w1_flag$flags

sensor_w2 <- window(clean(sensor_flag1, which = 1), n=300, type='rolling')
sensor_w2_flag <- flag(sensor_w2, 'x == 999999', 'persist(x) > 3', 'MAD(x,w) > 3', 'MAD(x) > 3')

df <- data_frame(
  times = sensor_w2_flag$sensor$times,
  x = sensor_w2_flag$sensor$x,
  MAD = MAD(sensor_w2_flag$sensor$x, sensor_w2_flag$sensor$w)
)
for (i in seq_along(sensor_w2_flag$flags)) {
  cat(i, sensor_w2_flag$flags[[i]]$expression, "\n")
  df[[sensor_w2_flag$flags[[i]]$expression]] <- FALSE
  df[[sensor_w2_flag$flags[[i]]$expression]][sensor_w2_flag$flags[[i]]$flag.i] <- TRUE
}

df %>%
  ggplot(aes(times, x, color = `MAD(x,w) > 3`)) +
  geom_point()


df %>%
  ggplot(aes(times, x, color = MAD > 3)) +
  geom_point()

df_input <- data_frame(
  times = seq.Date(from = as.Date("2000-01-01"), to = as.Date("2000-01-05"), by = "day"),
  x = rnorm(5)
)
s <- sensor(df_input)
flag(s, 'x > 0')
