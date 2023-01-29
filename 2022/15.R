library(tidyverse)
library(adventdrob)
input <- advent_input(15, 2022, parse = T)
input

## Functions: ####
no_beacon <- function(x, y, m_dist, goal) {
  out <- cbind(x, (y - m_dist):(y + m_dist)) %>%
    as_tibble(.name_repair = ~ c("x", "y"))

  rhombus <- c(1:m_dist, (m_dist - 1):1)
  goal_id <- which(out$y == goal)

  if(goal %in% out$y) {
    if(goal_id > 1 &&  goal_id <= length(rhombus)){
      i <- tibble(i = rhombus) %>%
        mutate(j = row_number() + 1) %>%
        filter(j == goal_id) %>%
        pull(i)
      out <- cbind((x - i):(x + i), goal) %>%
        as_tibble(.name_repair = ~ c("x", "y")) %>%
        rbind(out) %>%
        filter(y == goal) %>%
        unique()
    }
    return(out)
  } else {
    return(NA)
  }
}

# P1: ####
x <- input$x %>%
  str_extract_all("-?[:digit:]+") %>%
  tibble(x = .) %>%
  unnest_wider(
    x, names_repair = ~ c("sensor_x", "sensor_y", "beacon_x", "beacon_y")
  ) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(m_dist = abs(sensor_x - beacon_x) + abs(sensor_y - beacon_y))
beacons <- x %>%
  select(beacon_x, beacon_y) %>%
  unique() %>%
  rename(x = beacon_x, y = beacon_y)
out <- x %>%
  # takes too long otherwise:
  # rowwise() %>%
  # mutate(check = list((sensor_y - m_dist):(sensor_y + m_dist))) %>%
  # filter(2000000 %in% check) %>%
  # ungroup() %>%
  # back to regular scheduling:
  # mutate(out = pmap(list(sensor_x, sensor_y, m_dist), no_beacon)) %>%
  mutate(
    out = pmap(list(sensor_x, sensor_y, m_dist, 2000000), no_beacon)
  ) %>%
  unnest(out) %>%
  filter(!is.na(x)) %>%
  select(x, y) %>%
  unique() %>%
  anti_join(beacons, by = c("x", "y"))

out %>% filter(y == 2000000) %>% nrow()
# 4951427

# P2: ####
edge_detect <- function(x, y, m_dist) {
  cbind(x, (y - m_dist):(y + m_dist), c(0:m_dist, (m_dist - 1):0)) %>%
    as_tibble(.name_repair = ~ c("x", "y", "k")) %>%
    mutate(edge1 = x - k, edge2 = x + k) %>%
    select(y, edge1, edge2)
}

out <- x %>%
  mutate(out = pmap(list(sensor_x, sensor_y, m_dist), edge_detect)) %>%
  select(out) %>%
  unnest(out) %>%
  filter(y >= 0 & y <= 4000000) %>%
  arrange(desc(edge2)) %>%
  arrange(desc(edge1)) %>%
  arrange(y) %>%
  group_by(y) %>%
  mutate(n = row_number(),
         x = lag(edge1) - edge2)
# max(out$n) #13
out <- out %>%
  mutate(
    edge1 = if_else(edge1 < 0, 0, edge1),
    edge2 = if_else(edge2 > 4000000, 4000000, edge2),
    t = if_else(edge1 >= lag(edge1) & edge2 <= lag(edge2), 1, 0)
  ) %>%
  filter(t != 1) %>%
  mutate(n = row_number(), x = lag(edge1) - edge2)

out2 <- out %>% filter(x > 0) %>% arrange(desc(x))
# 3.37million
# out %>% filter(y == 308887)
# out %>% filter(y == 308888)
out3 <- out[which(out$y %in% out2$y), ] %>%
  rowwise() %>%
  mutate(x2 = list(edge1:edge2)) %>%
  group_by(y) %>%
  summarise(x = list(x2)) #%>%
  #mutate(x = map_dbl(x, ~ flatten_dbl(.x) %>% unique %>% length))
out3 %>%
  mutate(x = map_dbl(x, ~ flatten_dbl(.x) %>% unique %>% length)) %>%
  filter(x < 4000001)


for(i in 1:nrow(out2)) {
  if(i %% 100 == 0) print(paste0("round: ", i))
  z <- x %>%
    mutate(out = pmap(list(sensor_x, sensor_y, m_dist, out2$y[i]), no_beacon)) %>%
    unnest(out) %>%
    filter(!is.na(x)) %>%
    select(x, y) %>%
    filter(x >= 0 & x <= 4000000) %>%
    unique() %>%
    nrow()
  if (z < 4000001) {
    print(out2$y[i])
    break
  }
}
beacon_y <- 0

beacon_x <- x %>%
  mutate(out = pmap(list(sensor_x, sensor_y, m_dist, beacon_y), no_beacon)) %>%
  unnest(out) %>%
  filter(!is.na(x)) %>%
  select(x, y) %>%
  unique() %>%
  filter(x >= 0 & x <= 4000000) %>%
  pull(x) %>%
  setdiff(0:4000000, .)

beacon_x * 4000000 + beacon_y
#

###
## Test Case: ####
y <- readClipboard()
y <- str_extract_all(y, "-?[:digit:]+") %>%
  tibble(x = .) %>%
  unnest_wider(
    x, names_repair = ~ c("sensor_x", "sensor_y", "beacon_x", "beacon_y")
  ) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(m_dist = abs(sensor_x - beacon_x) + abs(sensor_y - beacon_y))

beacons <- y %>%
  select(beacon_x, beacon_y) %>%
  unique() %>%
  rename(x = beacon_x, y = beacon_y)
# no_beacon(8, 7, 9) %>% plot()

no_beacon <- function(x, y, m_dist, goal) {
  out <- cbind(x, (y - m_dist):(y + m_dist)) %>%
    as_tibble(.name_repair = ~ c("x", "y"))

  if(goal %in% out$y) {
    if(which(out$y == goal) > 1 && which(out$y == goal) <= length(c(1:m_dist, (m_dist - 1):1))){
      i <- tibble(i = c(1:m_dist, (m_dist - 1):1)) %>%
        mutate(j = row_number() + 1) %>%
        filter(j == which(out$y == goal)) %>%
        pull(i)
      out <- cbind((x - i):(x + i), goal) %>%
        as_tibble(.name_repair = ~ c("x", "y")) %>%
        rbind(out) %>%
        filter(y == goal) %>%
        unique()
    }
    return(out)
  } else {
    return(NA)
  }
}

for(i in 1:nrow(y)){
  print(i)
  df <- y[i,]
  no_beacon(df$sensor_x, df$sensor_y, df$m_dist, 10)
}

out <- y %>%
  # rowwise() %>%
  # mutate(check = list((sensor_y - m_dist):(sensor_y + m_dist))) %>%
  # filter(10 %in% check) %>%
  # ungroup() %>%
  mutate(
    out = pmap(list(sensor_x, sensor_y, m_dist, 10), no_beacon)
  ) %>%
  unnest(out) %>%
  filter(!is.na(x)) %>%
  select(x, y) %>%
  unique() %>%
  anti_join(beacons, by = c("x", "y"))
out %>% filter(y == 10) %>% nrow()

## P2:
y %>%
  mutate(out = pmap(list(sensor_x, sensor_y, m_dist), edge_detect)) %>%
  select(out) %>%
  unnest(out) %>%
  filter(y >= 0 & y <= 20) %>%
  arrange(desc(edge2)) %>%
  arrange(desc(edge1)) %>%
  arrange(y) %>%
  group_by(y) %>%
  mutate(n = row_number(),
         x = lag(edge1) - edge2) %>%
  filter(x > 0)

z <- y %>%
  mutate(out = pmap(list(sensor_x, sensor_y, m_dist), no_beacon_old)) %>%
  select(out) %>%
  unnest(out) %>%
  filter(y >= 0 & y <= 20) %>%
  filter(x >= 0 & x <= 20)
plot(z)

z <- y %>%
  select(-beacon_x, -beacon_y) %>%
  mutate(edges = pmap(list(sensor_x, sensor_y, m_dist), edge_detect),
         nobeacon = pmap(list(sensor_x, sensor_y, m_dist), no_beacon_old))

i <- 14
slice(z, i) %>%
  pull(edges) %>%
  .[[1]] %>%
  rowwise() %>%
  mutate(x = list(edge1:edge2)) %>%
  select(x, y) %>%
  unnest(x) %>%
  plot()
slice(z, i) %>% pull(nobeacon) %>% .[[1]] %>% plot()

y %>%
  mutate(edges = pmap(list(sensor_x, sensor_y, m_dist), edge_detect)) %>%
  select(edges) %>%
  unnest(edges) %>%
  filter(y >= 0 & y <= 20) %>%
  rowwise() %>%
  mutate(x = list(edge1:edge2)) %>%
  select(x, y) %>%
  unnest(x) %>%
  filter(x >= 0 & x <= 20) %>%
  plot()

z <- y %>%
  mutate(edges = pmap(list(sensor_x, sensor_y, m_dist), edge_detect)) %>%
  select(edges) %>%
  unnest(edges) %>%
  filter(y >= 0 & y <= 20)

z %>%
  arrange(desc(edge1)) %>%
  arrange(desc(edge2)) %>%
  arrange(y) %>%
  group_by(y) %>%
  mutate(t = case_when(
    edge1 >= lag(edge1) & edge2 <= lag(edge2) ~ 1,
    TRUE ~ 0
  )) %>%
  filter(t != 1) %>%
  mutate(n = row_number(), x = lag(edge1) - edge2) %>%
  filter(x > 0)



z %>%
  mutate(edge1 = if_else(edge1 < 0, 0, edge1),
         edge2 = if_else(edge2 > 20, 20, edge2)) %>%
  rowwise() %>%
  mutate(n = length(seq.int(edge1, edge2))) %>%
  arrange(y) %>%
  group_by(y) %>%
  summarise(n = sum(n))

# z %>%

#   group_by(y) %>%
#   mutate(n = row_number(),
#          x = lag(edge1) - edge2) %>%
#   filter(x > 0)



## notes ####
# 8:(8+9) #x
# 8:(8-9) #x
# 7:(7+9) #y
# 7:(7-9) #y
#
# a <- c((8+9), (7+9))
# b <- c((8+9), (7-9))
# c <- c((8-9), (7+9))
# d <- c((7+9), (7-9))
#
# +1-1
# +2-2
no_beacon_old <- function(x, y, m_dist) {
  out <- cbind(x, (y - m_dist):(y + m_dist)) %>%
    as_tibble(.name_repair = ~ c("x", "y"))
  j <- 1
  for(i in c(1:m_dist, (m_dist-1):1)) {
    out2 <- cbind((x - i):(x + i), out[j + 1,]$y) %>%
      as_tibble(.name_repair = ~ c("x", "y"))
    out <- rbind(out, out2)
    j <- j + 1
  }
  return(unique(out))
}
beacons <- x %>%
  select(beacon_x, beacon_y) %>%
  unique() %>%
  rename(x = beacon_x, y = beacon_y)
out <- x %>%
  # takes too long otherwise:
  rowwise() %>%
  mutate(check = list((sensor_y - m_dist):(sensor_y + m_dist))) %>%
  filter(2000000 %in% check) %>%
  ungroup() %>%
  # back to regular scheduling:
  mutate(out = pmap(list(sensor_x, sensor_y, m_dist), no_beacon)) %>%
  unnest(out) %>%
  select(x, y) %>%
  unique() %>%
  anti_join(beacons, by = c("x", "y"))

out %>% filter(y == 2000000) %>% nrow()

## P2:
beacon_x <= 4000000
beacon_x >= 0
beacon_y <= 4000000
beacon_y >= 0

2000000 <= 4000000 # T
out <- x %>%
  mutate(
    out = pmap(list(sensor_x, sensor_y, m_dist, 2000000), no_beacon)
  ) %>%
  unnest(out) %>%
  filter(!is.na(x)) %>%
  select(x, y) %>%
  unique()
out %>% filter(x >= 0 & x <= 4000000)

out <- x %>%
  select(sensor_x, sensor_y, m_dist) %>%
  rowwise() %>%
  mutate(y_check = list((sensor_y - m_dist):(sensor_y + m_dist))) %>%
  unnest(y_check) %>%
  filter(y_check >= 0 & y_check <= 4000000) %>%
  chop(y_check) #32,017,999
out <- out %>%
  mutate(y = map(y_check, ~ setdiff(0:4000000, .x)))
out <- out %>%
  rowwise() %>%
  mutate(x_check = list((sensor_x - m_dist):(sensor_x + m_dist))) %>%
  unnest(x_check) %>%
  filter(x_check >= 0 & x_check <= 4000000) %>%
  chop(x_check) %>%
  mutate(x = map(x_check, ~ setdiff(0:4000000, .x)))

# rhombus = list(c(1:m_dist, (m_dist - 1):1))


# out %>%
#   group_by(sensor_x) %>%
#   summarise()

#rowwise() %>% mutate(x = list(edge1:edge2))
out2 <- out[1:20,]
rm(out)

out2 <- out %>%
  group_by(y) %>%
  mutate(x = lag(edge1) - edge2) %>%
  filter(x >= 0)

#     y   edge1   edge2
# <dbl>   <dbl>   <dbl>
#     0 3220524 4757126
#     0  873018 3984974
#     0   70058 1218708
#     0   70058  669264
#     0 -529422  659212
