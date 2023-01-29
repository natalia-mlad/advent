library(tidyverse)
library(adventdrob)
input <- advent_input(18, 2022, parse = T)
input <- input %>%
  separate(x, c("x", "y", "z"), convert = T)

# Functions: ####
count_connected <- function(df) {
  connected <- 0
  # inefficient but works (TODO)
  for(i in 1:nrow(df)) {
    print(i)
    # alongside x:
    m1 <- df %>%
      filter(y == df$y[i] & z == df$z[i]) %>%
      filter(x != df$x[i])
    if (any(df$x[i] - 1 == m1$x)) {
      connected <- connected + 1
    }
    if (any(df$x[i] + 1 == m1$x)) {
      connected <- connected + 1
    }

    # alongside z:
    m2 <- df %>%
      filter(y == df$y[i] & x == df$x[i]) %>%
      filter(z != df$z[i])
    if (any(df$z[i] - 1 == m2$z)) {
      connected <- connected + 1
    }
    if (any(df$z[i] + 1 == m2$z)) {
      connected <- connected + 1
    }

    # alongside y:
    m3 <- df %>%
      filter(z == df$z[i] & x == df$x[i]) %>%
      filter(y != df$y[i])
    if (any(df$y[i] - 1 == m3$y)) {
      connected <- connected + 1
    }
    if (any(df$y[i] + 1 == m3$y)) {
      connected <- connected + 1
    }
  }
  return(connected)
}

plot_rec <- function(df) {
  plot(c(0, 20), c(0, 20), type = "n")
  rect(df$x, df$y, df$x + 1, df$y + 1, col = "black")
}

## Part 1: ####
# nrow(unique(input)) - nrow(input)
connected <- count_connected(input)
(surface_area <- nrow(input) * 6 - connected)
# 3494

## Part 2: ####
# What is the exterior surface area of your scanned lava droplet?
# TODO:
# airpockets <- count_airpockets(input)
# airpockets <- 1432
# surface_area - airpockets
# 2062


# Example: ####
# 1x1x1 cubes
# To approximate the surface area,
# count the number of sides of each cube
# that are not immediately connected to another cube
# each cube has 6 sides (if connected -1)
##
# So, if your scan were only two adjacent cubes like 1,1,1 and 2,1,1,
# each cube would have a single side covered and five sides exposed,
# a total surface area of 10 sides.
example <- tribble(
  ~ x, ~ y, ~ z,
  1, 1, 1,
  2, 1, 1
)
out <- example
for (i in 1:nrow(example)) {
  out <- tibble(x = example$x[i] + 1, y = example$y[i] + 1, z = example$z[i] + 1) %>%
    rbind(out, .)
}
rgl::plot3d(x = out$x, y = out$y, z = out$z, type = "s")

##
example <- readClipboard() %>%
  tibble(x = .) %>%
  separate(x, c("x", "y", "z"), convert = T)
# example %>% arrange(y, z) %>%
#   mutate(m1 = if_else(y == z, 1, 0), m2 = if_else(x == y, 1, 0), m3 = if_else(z == x, 1, 0))

nrow(example) * 6 - count_connected(example)

## Part 2:
# your calculation also included the surface area of air pockets
# trapped in the lava droplet.
#
# Instead, consider only cube sides that could be reached
# by the water and steam as the lava droplet tumbles into the pond.
# The steam will expand to reach as much as possible,
# completely displacing any air on the outside of the lava droplet but never expanding diagonally.
##
# Exactly one cube of air is trapped within the lava droplet (at 2,2,5),
# so the exterior surface area of the lava droplet is 58.
##
example %>% filter(x == 2 & y == 2) %>% arrange(z)
s <- example %>% filter(z == 5) %>% arrange(y) %>% arrange(x)
s %>%
  expand(x, y, z) %>%
  filter(x != max(s$x) & x != min(s$x)) %>%
  filter(y != max(s$y) & y != min(s$y)) %>%
  anti_join(s, by = c("x", "y", "z"))
s <- example %>% filter(x == 2 & y == 2)
##
count_airpockets <- function(df) {
  air <- 0

  all_z <- sort(unique(df$z))
  for (i in 2:(length(all_z) - 1)) {
    s <- df %>% filter(z == all_z[i])
    out <- s %>%
      expand(x, y, z) %>%
      filter(x != max(s$x) & x != min(s$x)) %>%
      filter(y != max(s$y) & y != min(s$y)) %>%
      anti_join(s, by = c("x", "y", "z"))
    if (nrow(out) > 0) {
      air <- air + nrow(out)
      # s <- df %>% filter(x == out$x & y == out$y)
    }
  }
  # df_z <- df %>% select(x, y) %>% unique()
  # for(i in 1:nrow(df_z)) {
  #   z1 <- df %>% filter(y == df_z$y[i] & x == df_z$x[i]) %>% pull(z)
  #   out <- min(z1):max(z1) %>% setdiff(z1) %>% length()
  #   air <- air + out
  # }
  return(air)
}

64 - count_airpockets(example) * 6

# example %>% filter(z == 2) %>% select(x, y) %>% plot()
# df <- example %>% filter(z == 6)
df <- input %>% filter(z == all_z[2])
nrow(df)
plot(c(0, 20), c(0, 20), type = "n")
rect(df$x, df$y, df$x + 1, df$y + 1, col = "black")
# xleft, ybottom, xright, ytop

## Cool Plot: ##
all_z <- sort(unique(input$z))
for (i in 2:(length(all_z) - 1)) {
  df <- input %>% filter(z == all_z[i])
  # print(nrow(df))
  # print(all_z[i])
  plot(c(0, 20), c(0, 20), type = "n", main = paste0("z = ", all_z[i]))
  rect(df$x, df$y, df$x + 1, df$y + 1, col = "black")
  readline(prompt = "Press [any button] to continue")
}



# see z == 5:7
s <- input %>% filter(z == 1)
s2 <- s %>%
  group_by(y) %>%
  summarise(min_x = min(x), max_x = max(x))
miss <- s %>%
  expand(x, y, z) %>%
  filter(x != max(s$x) & x != min(s$x)) %>%
  filter(y != max(s$y) & y != min(s$y)) %>%
  anti_join(s, by = c("x", "y", "z"))
# miss %>% anti_join(s2 %>% rename(x = max_x))
miss
plot_rec(s)
plot_rec(miss)

x = 9
y = 11

m1 <- df %>%
  filter(y == df$y[i] & z == df$z[i]) %>%
  filter(x != df$x[i])
if (any(df$x[i] - 1 == m1$x)) {
  connected <- connected + 1
}
if (any(df$x[i] + 1 == m1$x)) {
  connected <- connected + 1
}

#### Another: ####
coords <- as.matrix(input)
dd <- max(coords) + 1
droplets <- array(F, dim = rep(dd + 2, 3))
droplets[coords + 2] <- T

# all the coords exclude left and right most
shifty <- function(dim, shift = 0){
  cc <- rep(list(1:(dd+2)), 3)
  cc[[dim]] <- 1:(dd+1) + shift
  do.call(expand.grid, cc) %>% as.matrix()
}

surface_area <- function(mm){
  adj <- function(dim){
    map(0:1, ~mm[shifty(dim, .)]) %>%
      reduce(`&`) %>%
      sum()
  }
  borders <- map_dbl(1:3, adj) %>% sum()
  sum(mm) * 6 - borders * 2
}

# fill the boundaries of the droplets with steam:
steam <- array(F, dim = rep(dd + 2, 3))
steam[1,,] = steam[,1,] = steam[,,1] = T
steam[dd + 2,,] = steam[,dd + 2,] = steam[,,dd + 2] = T

nsteam = c(sum(steam), 0)
while(nsteam[1] > nsteam[2]){
  steam0 <- steam
  for(dim in 1:3){
    superpos <- map(0:1, ~ steam0[shifty(dim, .)]) %>%
      reduce(`|`)
    steam[shifty(dim, 0)] <- superpos | steam[shifty(dim, 0)]
    steam[shifty(dim, 1)] <- superpos | steam[shifty(dim, 1)]
  }
  steam <- steam & !droplets
  nsteam <- c(sum(steam), nsteam)
}

surface_area(droplets | !steam & !droplets)
# 2062
