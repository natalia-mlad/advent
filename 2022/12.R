library(tidyverse)
library(adventdrob)
x <- advent_input(12, 2022, parse = T)

## Part 1: ####
x <- x %>%
  grid_tidy(x) %>%
  mutate(
    start = if_else(value == "S", 1, 0),
    end = if_else(value == "E", 1, 0),
    value = if_else(value == "S", "a", value),
    value = if_else(value == "E", "z", value),
    value = letters_to_numbers(value)
  )
find_path(x)


## Part 2: ####


## notes ####
y <- readClipboard()
y <- tibble(x = y)
y <- y %>%
  grid_tidy(x) %>%
  mutate(
    start = if_else(value == "S", 1, 0),
    end = if_else(value == "E", 1, 0),
    value = if_else(value == "S", "a", value),
    value = if_else(value == "E", "z", value),
    value = letters_to_numbers(value)
  )
# During each step, move exactly one square up, down, left, or right.
# the elevation of the destination square can be at most one higher than the elevation of your current square
##
# marks for your current position (S)
# and the location that should get the best signal (E).
# Your current position (S) has elevation a,
# and the location that should get the best signal (E) has elevation z.

i <- which(y$start == 1)
out <- c()
out <- c(out, i)
while(i != which(y$end == 1)) {
  if (y$row[i] - 1 >= min(y$row)) {
    new_row <- y$row[i] - 1
    new_val <- y %>%
      filter(row == new_row & col == y$col[i]) %>%
      pull(value)
    if (new_val - y$value[i] <= 1) {
      new_loc <- which(y$row == new_row & y$col == y$col[i])
      if (!new_loc %in% out) {
        i <- new_loc
        out <- c(out, i)
        next
      }
    }
  }
  if (y$col[i] + 1 <= max(y$col)) {
    new_col <- y$col[i] + 1
    new_val <- y %>%
      filter(row == y$row[i] & col == new_col) %>%
      pull(value)
    if (new_val - y$value[i] <= 1) {
      new_loc <- which(y$row == y$row[i] & y$col == new_col)
      if (!new_loc %in% out) {
        i <- new_loc
        out <- c(out, i)
        next
      }
    }
  }
  if (y$col[i] - 1 >= min(y$col)) {
    new_col <- y$col[i] - 1
    new_val <- y %>%
      filter(row == y$row[i] & col == new_col) %>%
      pull(value)
    if (new_val - y$value[i] <= 1) {
      new_loc <- which(y$row == y$row[i] & y$col == new_col)
      if (!new_loc %in% out) {
        i <- new_loc
        out <- c(out, i)
        next
      }
    }
  }
  if (y$row[i] + 1 <= max(y$row)) {
    new_row <- y$row[i] + 1
    new_val <- y %>%
      filter(row == new_row & col == y$col[i]) %>%
      pull(value)
    if (new_val - y$value[i] <= 1) {
      new_loc <- which(y$row == new_row & y$col == y$col[i])
      if (!new_loc %in% out) {
        i <- new_loc
        out <- c(out, i)
        next
      }
    }
  }
  break
}
if(which(y$end == 1) %in% out) length(out)
#40

# new - old
# 1 - 13 <= 1
# 14 - 13 <= 1
# 15 - 13 <= 1

next_step <- function(y, i){
  new_loc <- c()
  if (y$row[i] + 1 <= max(y$row)) {
    new_row <- y$row[i] + 1
    new_val <- y %>%
      filter(row == new_row & col == y$col[i]) %>%
      pull(value)
    if (new_val - y$value[i] <= 1) {
      new_loc <- c(new_loc, which(y$row == new_row & y$col == y$col[i]))
    }
  }
  if (y$row[i] - 1 >= min(y$row)) {
    new_row <- y$row[i] - 1
    new_val <- y %>%
      filter(row == new_row & col == y$col[i]) %>%
      pull(value)
    if (new_val - y$value[i] <= 1) {
      new_loc <- c(new_loc, which(y$row == new_row & y$col == y$col[i]))
    }
  }
  if (y$col[i] + 1 <= max(y$col)) {
    new_col <- y$col[i] + 1
    new_val <- y %>%
      filter(row == y$row[i] & col == new_col) %>%
      pull(value)
    if (new_val - y$value[i] <= 1) {
      new_loc <- c(new_loc, which(y$row == y$row[i] & y$col == new_col))
    }
  }
  if (y$col[i] - 1 >= min(y$col)) {
    new_col <- y$col[i] - 1
    new_val <- y %>%
      filter(row == y$row[i] & col == new_col) %>%
      pull(value)
    if (new_val - y$value[i] <= 1) {
      new_loc <- c(new_loc, which(y$row == y$row[i] & y$col == new_col))
    }
  }
  return(new_loc)
}

z <- y %>%
  mutate(id = row_number(),
         next_step = map(id, ~ next_step(y, .x))) %>%
  select(id, next_step, start, end) %>%
  unnest(next_step)

i <- which(y$end == 1)
out <- c()
for (k in 1:33) {
  out <- c(out, i)
  i <- z %>%
    filter(next_step == i) %>%
    pull(id) %>%
    head(1)
}




find_path <- function(y) {
  end <- which(y$end == 1)
  i <- which(y$start == 1)
  out <- c()
  out <- c(out, i)
  while (i != end) {
    new_loc <- next_step(y, i)
    # new_loc <- new_loc[!new_loc %in% which(table(c(out, new_loc)) > 2)]
    new_loc <- new_loc[!new_loc %in% out] #don't revisit
    if (length(new_loc) == 0) break
    # minimize distance approach:
    # i <- y[new_loc, c("row", "col")] %>%
    #   rename(row_loc = row, col_loc = col) %>%
    #   cbind(y[end, c("row", "col")]) %>%
    #   cbind(new_loc) %>%
    #   tibble() %>%
    #   arrange(row - row_loc + col - col_loc) %>%
    #   slice(1) %>% pull(new_loc)
    # i <- tibble(new_loc) %>% arrange(end - new_loc) %>%
    #   slice(1) %>% pull(new_loc)
    # i <- max(new_loc) #34
    # i <- sample(new_loc, 1)
    i <- new_loc %>% head(5) %>% sample(1)
    out <- c(out, i)
    if (length(out) > nrow(y)) break
  }
  return(out)
  # if(end %in% out) length(out)
}

# out <- find_path(y)
# y[out, ] %>% select(row, col)
# 1  9 17 25 33 34 33 34 33 34 33 34 33 34 33 34 33 34 33 34 33 34 33 34

out <- c()
# for (i in 1:5) {out <- c(out, find_path(y))}
while(length(out) == 0) out <- c(out, find_path(y))
out

#### another solution ####
# input_mat <- do.call(rbind, lapply(readClipboard(), utf8ToInt))
library(tidygraph)

g <- input %>% grid_graph(x, mutual = T, directed = T)
nodes <- as_tibble(g) %>%
  mutate(elevation = case_when(value == "S" ~ 1L,
                               value == "E" ~ 26L,
                               TRUE ~ match(value, letters)))
filt <- g %>%
  activate("edges") %>%
  mutate(from_val = nodes$elevation[from],
         to_val = nodes$elevation[to]) %>%
  filter(to_val <= from_val + 1) %>%
  activate("nodes") %>%
  mutate(distance = node_distance_to(which(nodes$value == "E"), mode = "out"))

# P1:
filt %>%
  filter(value == "S")
# 468

# P2:
filt %>%
  filter(value == "a") %>%
  arrange(distance)
# 459
