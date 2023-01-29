library(tidyverse)
library(adventdrob)
input <- advent_input(14, 2022, parse = T)

## Functions: ####
complete_axis <- function(out) {
  out2 <- matrix(ncol = 2)
  for (i in 1:(nrow(out) - 1)) {
    out2 <- cbind(out$x[i]:out$x[i + 1], out$y[i]:out$y[i + 1]) %>%
      rbind(out2, .)
    if(i != nrow(out) - 1) out2 <- head(out2, -1)
  }
  out2 <- out2[-1, ] %>%
    as_tibble(.name_repair = ~ c("x", "y"))
  return(out2)
}

tidy_input <- function(x) {
  str_split(x, " -> ")[[1]] %>%
    tibble(x = .) %>%
    separate(x, c("x", "y"), convert = T) %>%
    complete_axis()
}

## Part 1: ####
z <- input %>%
  mutate(out = map(x, tidy_input)) %>%
  select(out) %>%
  unnest(out) %>%
  mutate(y = -y)
# plot(z)
void <- min(z$y)
sand_grain <- 0
sand_coord <- c(500, 0)
while(sand_coord[2] >= void) {
  sand_coord <- c(500, 0)
  d_test <- !any(sand_coord[1] == z$x & sand_coord[2] - 1 == z$y)
  dl_test <- !any(sand_coord[1] - 1 == z$x & sand_coord[2] - 1 == z$y)
  dr_test <- !any(sand_coord[1] + 1 == z$x & sand_coord[2] - 1 == z$y)
  while (any(d_test, dl_test, dr_test)) {
    if (d_test) {
      # falls down one step:
      sand_coord[2] <- sand_coord[2] - 1
      # if falls lower than the lowest y-axis then abyss:
      if (sand_coord[2] < void) break
    } else if (dl_test) {
      # down-left:
      sand_coord[2] <- sand_coord[2] - 1
      sand_coord[1] <- sand_coord[1] - 1
    } else if (dr_test) {
      # down-right:
      sand_coord[2] <- sand_coord[2] - 1
      sand_coord[1] <- sand_coord[1] + 1
    }
    d_test <- !any(sand_coord[1] == z$x & sand_coord[2] - 1 == z$y)
    dl_test <- !any(sand_coord[1] - 1 == z$x & sand_coord[2] - 1 == z$y)
    dr_test <- !any(sand_coord[1] + 1 == z$x & sand_coord[2] - 1 == z$y)
  }
  # if falls lower than the lowest y-axis then abyss:
  if (sand_coord[2] < void) break
  # sand comes to rest:
  sand_grain <- sand_grain + 1
  # print(sand_grain); print(sand_coord)
  z <- z %>% rbind(sand_coord)
}
# plot(z)
sand_grain # 719

## Part 2: ####
# range(z$x) #488 552
z <- input %>%
  mutate(out = map(x, tidy_input)) %>%
  select(out) %>%
  unnest(out) %>%
  mutate(y = -y) %>%
  # the floor:
  # 10 is too high; 8 works but slow
  rbind(complete_axis(tibble(
    x = c(-10 ^ 3, 10 ^ 3), y = c(void - 2, void - 2)
  )))
# tibble(x = c(-Inf, Inf), y = c(void - 2, void - 2))

sand_grain <- 0
sand_coord <- c(500, -1)
while(sand_coord[2] < 0) {
  sand_coord <- c(500, 0)
  d_test <- !any(sand_coord[1] == z$x & sand_coord[2] - 1 == z$y)
  dl_test <- !any(sand_coord[1] - 1 == z$x & sand_coord[2] - 1 == z$y)
  dr_test <- !any(sand_coord[1] + 1 == z$x & sand_coord[2] - 1 == z$y)
  while (any(d_test, dl_test, dr_test)) {
    if (d_test) {
      # falls down one step:
      sand_coord[2] <- sand_coord[2] - 1
      if (sand_coord[2] < (void - 2)) {
        warning("Floor wasn't wide enough!")
        break
      }
    } else if (dl_test) {
      # down-left:
      sand_coord[2] <- sand_coord[2] - 1
      sand_coord[1] <- sand_coord[1] - 1
    } else if (dr_test) {
      # down-right:
      sand_coord[2] <- sand_coord[2] - 1
      sand_coord[1] <- sand_coord[1] + 1
    }
    d_test <- !any(sand_coord[1] == z$x & sand_coord[2] - 1 == z$y)
    dl_test <- !any(sand_coord[1] - 1 == z$x & sand_coord[2] - 1 == z$y)
    dr_test <- !any(sand_coord[1] + 1 == z$x & sand_coord[2] - 1 == z$y)
  }
  if (sand_coord[2] == 0) break
  if (sand_coord[2] < (void - 2)) {
    warning("Floor wasn't wide enough!")
    break
  }
  # sand comes to rest:
  sand_grain <- sand_grain + 1
  print(sand_grain)
  z <- z %>% rbind(sand_coord)
}

sand_grain + 1 # 23390

###
## Test Case: ####
y <- readClipboard()
y1 <- y[1] %>%
  str_split(" -> ") %>%
  .[[1]] %>%
  tibble(x = .) %>%
  separate(x, c("x", "y"), convert = T) %>%
  complete_axis()
y2 <- y[2] %>%
  str_split(" -> ") %>%
  .[[1]] %>%
  tibble(x = .) %>%
  separate(x, c("x", "y"), convert = T) %>%
  complete_axis()

z <- rbind(y1, y2) %>% mutate(y = -y)
void <- min(z$y)

sand_grain <- 0
sand_coord <- c(500, 0)
while(sand_coord[2] >= void) {
  sand_coord <- c(500, 0)
  d_test <- !any(sand_coord[1] == z$x & sand_coord[2] - 1 == z$y)
  dl_test <- !any(sand_coord[1] - 1 == z$x & sand_coord[2] - 1 == z$y)
  dr_test <- !any(sand_coord[1] + 1 == z$x & sand_coord[2] - 1 == z$y)
  while (any(d_test, dl_test, dr_test)) {
    if (d_test) {
      # falls down one step:
      sand_coord[2] <- sand_coord[2] - 1
      # if falls lower than the lowest y-axis then abyss:
      if (sand_coord[2] < void) break
    } else if (dl_test) {
      # down-left:
      sand_coord[2] <- sand_coord[2] - 1
      sand_coord[1] <- sand_coord[1] - 1
    } else if (dr_test) {
      # down-right:
      sand_coord[2] <- sand_coord[2] - 1
      sand_coord[1] <- sand_coord[1] + 1
    }
    d_test <- !any(sand_coord[1] == z$x & sand_coord[2] - 1 == z$y)
    dl_test <- !any(sand_coord[1] - 1 == z$x & sand_coord[2] - 1 == z$y)
    dr_test <- !any(sand_coord[1] + 1 == z$x & sand_coord[2] - 1 == z$y)
  }
  # if falls lower than the lowest y-axis then abyss:
  if (sand_coord[2] < void) break
  # sand comes to rest:
  sand_grain <- sand_grain + 1
  # print(sand_grain); print(sand_coord)
  z <- z %>% rbind(sand_coord)
}
plot(z)

####
## P2:
z <- rbind(y1, y2) %>% mutate(y = -y)
void <- min(z$y)
z <- z %>%
  rbind(complete_axis(tibble(
    x = c(-10 ^ 6, 10 ^ 6), y = c(void - 2, void - 2)
  )))

sand_grain <- 0
sand_coord <- c(500, -1)
while(sand_coord[2] < 0) {
  sand_coord <- c(500, 0)
  d_test <- !any(sand_coord[1] == z$x & sand_coord[2] - 1 == z$y)
  dl_test <- !any(sand_coord[1] - 1 == z$x & sand_coord[2] - 1 == z$y)
  dr_test <- !any(sand_coord[1] + 1 == z$x & sand_coord[2] - 1 == z$y)
  while (any(d_test, dl_test, dr_test)) {
    if (d_test) {
      # falls down one step:
      sand_coord[2] <- sand_coord[2] - 1
    } else if (dl_test) {
      # down-left:
      sand_coord[2] <- sand_coord[2] - 1
      sand_coord[1] <- sand_coord[1] - 1
    } else if (dr_test) {
      # down-right:
      sand_coord[2] <- sand_coord[2] - 1
      sand_coord[1] <- sand_coord[1] + 1
    }
    d_test <- !any(sand_coord[1] == z$x & sand_coord[2] - 1 == z$y)
    dl_test <- !any(sand_coord[1] - 1 == z$x & sand_coord[2] - 1 == z$y)
    dr_test <- !any(sand_coord[1] + 1 == z$x & sand_coord[2] - 1 == z$y)
  }
  if (sand_coord[2] == 0) break
  # sand comes to rest:
  sand_grain <- sand_grain + 1
  print(sand_grain)
  z <- z %>% rbind(sand_coord)
}

sand_grain + 1
