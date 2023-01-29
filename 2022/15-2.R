library(tidyverse)
library(adventdrob)
input <- advent_input(15, 2022, parse = T)
input

beacons <- input %>%
  extract(x,
          c("sensor_x", "sensor_y", "beacon_x", "beacon_y"),
          "Sensor at x=(.*), y=(.*): closest beacon is at x=(.*), y=(.*)",
          convert = T) %>%
  mutate(dist = abs(sensor_x - beacon_x) + abs(sensor_y - beacon_y))

intervals_ruled_out <- beacons %>%
  crossing(y = 0:4e6) %>%
  mutate(max_x_dist = dist - abs(sensor_y - y)) %>%
  filter(max_x_dist >= 0) %>%
  mutate(start = pmax(0, sensor_x - max_x_dist),
         end = pmin(4e6, sensor_x + max_x_dist))

intervals_ruled_out %>%
  arrange(y, start) %>%
  mutate(next_start = lead(start)) %>%
  filter(y == lead(y)) %>%
  group_by(y) %>%
  mutate(highest_end = cummax(end)) %>%
  ungroup() %>%
  filter(highest_end < next_start - 1) %>%
  transmute(x = next_start - 1, y) %>%
  mutate(result = x * 4e6 + y)
# x = 3257428
# y = 2573243
# 13029714573243
