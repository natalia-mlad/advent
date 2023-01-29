library(tidyverse)
library(adventdrob)
# start <- Sys.time()
input <- advent_input(9, 2022, parse = T)
input <- input %>%
  separate(x, c("dir", "n"), convert = T)

## Functions: ####
move_h <- function(motions) {
  h_pos <- tibble(x = 0, y = 0)
  for (j in 1:nrow(motions)) {
    n <- motions$n[j]
    dir <- motions$dir[j]
    for (i in 1:n) {
      if (dir == "R") {
        new_x <- tail(h_pos$x, 1) + 1
        new_y <- tail(h_pos$y, 1)
      } else if (dir == "L") {
        new_x <- tail(h_pos$x, 1) - 1
        new_y <- tail(h_pos$y, 1)
      } else if (dir == "U") {
        new_x <- tail(h_pos$x, 1)
        new_y <- tail(h_pos$y, 1) + 1
      } else if (dir == "D") {
        new_x <- tail(h_pos$x, 1)
        new_y <- tail(h_pos$y, 1) - 1
      }
      # output:
      h_pos <- rbind(h_pos, c(new_x, new_y))
    }
  }
  return(h_pos)
}

move_t <- function(h_pos) {
  t_pos <- tibble(x = 0, y = 0)
  for (i in 1:nrow(h_pos)) {
    cur_x <- tail(t_pos$x, 1)
    cur_y <- tail(t_pos$y, 1)
    h_x <- h_pos$x[i]
    h_y <- h_pos$y[i]
    # not touching:
    if (abs(h_x - cur_x) > 1 | abs(h_y - cur_y) > 1) {
      # diagonal move:
      if (cur_x != h_x && cur_y != h_y) {
        if (cur_x < h_x) {
          new_x <- cur_x + 1
        } else if (h_x < cur_x) {
          new_x <- cur_x - 1
        }
        if (h_y < cur_y) {
          new_y <- cur_y - 1
        } else if (cur_y < h_y) {
          new_y <- cur_y + 1
        }
      # straight move:
      } else if (h_x - cur_x > 1) {
        new_x <- cur_x + 1
        new_y <- cur_y
      } else if (cur_x - h_x > 1) {
        new_x <- cur_x - 1
        new_y <- cur_y
      } else if (h_y - cur_y > 1) {
        new_x <- cur_x
        new_y <- cur_y + 1
      } else if (cur_y - h_y > 1) {
        new_x <- cur_x
        new_y <- cur_y - 1
      }
    # touching:
    } else {
      new_x <- tail(t_pos$x, 1)
      new_y <- tail(t_pos$y, 1)
    }
    # output:
    t_pos <- rbind(t_pos, c(new_x, new_y))
  }
  return(t_pos[-1, ])
}

## Part 1: ####
# the head (H) and tail (T) must always be touching
# (diagonally adjacent and even overlapping both count as touching)
# If the head is ever two steps directly up, down, left, or right from the tail,
# the tail must also move one step in that direction so it remains close enough
# Otherwise, if the head and tail aren't touching and aren't in the same row or column,
# the tail always moves one step diagonally to keep up.
# How many positions does the tail of the rope visit at least once?
h_pos <- move_h(input)
t_pos <- move_t(h_pos)
nrow(h_pos) == nrow(t_pos)
nrow(unique(t_pos)) # 6236
# Sys.time() - start
# took about an hour (but I did a lot of extra visualisations)


## Part 2: ####
start <- Sys.time()
# Each knot further down the rope follows the knot in front of it
# using the same rules as before.
for (i in 1:8) {
  t_pos <- move_t(t_pos)
}
nrow(unique(t_pos)) #2449
# Sys.time() - start
# took 5 min


### Test Case: ####
y <- readClipboard()
y <- tibble(x = y) %>%
  separate(x, c("dir", "n"), convert = T)
h_pos <- move_h(y)
t_pos <- move_t(h_pos)
nrow(h_pos) == nrow(t_pos)
nrow(unique(t_pos)) == 13




# NOTES ####
# right, x+
# left x-
# up y+
# down y-

# move_h <- function(n, dir, h_pos)
debugonce(move_t)
move_t(h_pos)
t_pos[3, ]
h_pos[3, ]

for(i in 6:10) {
  cat(i)
  rbind(h_pos[i,], t_pos[i,]) %>%
    plot(xlim = c(0, 5), ylim = c(0, 5), pch = c("H", "T"))
  # readline(prompt = "Press [any button] to continue")
}
# 7
cur_x = 3
cur_y = 0
h_pos$x[i] = 4
h_pos$y[i] = 2

for (i in 1:8) {
  # one_pos <- t_pos
  # two_pos <- move_t(one_pos)
  print(i+1)
}


# Visualise: ####
# set-up:
h_pos <- move_h(input)
t_pos <- move_t(h_pos)

# create & save the images:
rbind(h_pos, t_pos) %>% pull(x) %>% range() #xlim
rbind(h_pos, t_pos) %>% pull(y) %>% range() #ylim
for(i in 1:nrow(h_pos)) {
  png(sprintf("day9_images/f%05d.png", i))
  rbind(h_pos[i,], t_pos[i,]) %>%
    plot(
      xlim = c(-105, 55),
      ylim = c(-275, 10),
      pch = c("H", "T")
    )
  dev.off()
}

# gif:
library(gifski)
nrow(h_pos) / 10 / 60 # 19+min
images_list <- list.files(path = "day9_images/", pattern = '*.png', full.names = T)
images_list %>%
  gifski(gif_file = "day9_p1.gif", delay = 0.1)


###
# too slow:
# library(magick)
# my_img <- images_list[1:600] %>%
#   # images_list %>%
#   image_read() %>%
#   image_join()
# my_gif <- image_animate(my_img, fps = 10)
# image_write(my_gif, "day9_p1.gif")
# # optimize = T
