library(tidyverse)
library(adventdrob)
input <- advent_input(17, 2022, parse = T)
input <- str_split(input$x, "")[[1]]
# input <- str_split(">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>", "")[[1]]

## Functions: ####
draw_shape <- function(s, x_l, x_r, y) {
  if (s == 1) {
    shape_coord <- cbind(x = x_l:x_r, y) %>% as_tibble()
  } else if (s == 2) {
    shape_coord <- cbind(x = x_l:x_r, y = y + 1) %>%
      rbind(c((x_l + x_r) / 2, y)) %>%
      rbind(c((x_l + x_r) / 2, y + 2)) %>%
      as_tibble()
  } else if (s == 3) {
    shape_coord <- cbind(x = x_l:x_r, y) %>%
      rbind(cbind(x = x_r, y = (y + 1):(y + 2))) %>%
      as_tibble()
  } else if (s == 4) {
    shape_coord <- cbind(x = x_l, y = y:(y + 3)) %>% as_tibble()
  } else if (s == 5) {
    shape_coord <- cbind(x = x_l:x_r, y = y) %>%
      rbind(c(x_l, y + 1)) %>%
      rbind(c(x_r, y + 1)) %>%
      as_tibble()
  }
  return(shape_coord)
}

## Part 1: #####
min_x = 1
max_x = 7
i <- 1
jet <- input[i]
out <- cbind(x = min_x:max_x, y = 0) %>% as_tibble()
shapes <- c(1:5)
j <- 1

for(k in 1:1000000000000) {
  # rock appears:
  y <- max(out$y) + 4
  x_l <- 3
  if(shapes[j] == 1) {
    x_r = x_l + 3
  } else if (shapes[j] == 2 | shapes[j] == 3) {
    x_r = x_l + 2
  } else if (shapes[j] == 4) {
    x_r = x_l
  } else if (shapes[j] == 5) {
    x_r = x_l + 1
  }

  while(TRUE) {
    # jet of gas:
    if (jet == ">") {
      test <-  draw_shape(shapes[j], x_l + 1, x_r + 1, y) %>%
        intersect(out) %>% nrow()
      if (x_r + 1 <= max_x && test == 0) {
        x_r <- x_r + 1
        x_l <- x_l + 1
      }
    } else if (jet == "<") {
      test <- draw_shape(shapes[j], x_l - 1, x_r - 1, y) %>%
        intersect(out) %>% nrow()
      if (x_l - 1 >= min_x && test == 0) {
        x_l <- x_l - 1
        x_r <- x_r - 1
      }
    }
    i <- i + 1
    if (i > length(input)) i <- 1
    jet <- input[i]

    # fall 1 unit:
    y <- y - 1
    shape_coord <- draw_shape(shapes[j], x_l, x_r, y)

    # ready to settle?
    if (nrow(intersect(shape_coord, out)) > 0) {
      y <- y + 1
      shape_coord <- draw_shape(shapes[j], x_l, x_r, y)
      out <- rbind(shape_coord, out)
      break
    }
  }

  j <- j + 1
  if (j > length(shapes)) j <- 1
}

max(out$y) # 3217


## Part 2: #####
out2 <- list()
m <- 0
min_x = 1
max_x = 7
i <- 1
jet <- input[i]
out <- cbind(x = min_x:max_x, y = 0) %>% as_tibble()
shapes <- c(1:5)
j <- 1
for(k in 1:2022) {
  # rock appears:
  y <- max(out$y) + 4
  x_l <- 3
  if(shapes[j] == 1) {
    x_r = x_l + 3
  } else if (shapes[j] == 2 | shapes[j] == 3) {
    x_r = x_l + 2
  } else if (shapes[j] == 4) {
    x_r = x_l
  } else if (shapes[j] == 5) {
    x_r = x_l + 1
  }
  while(TRUE) {
    # jet of gas:
    if (jet == ">") {
      test <-  draw_shape(shapes[j], x_l + 1, x_r + 1, y) %>%
        intersect(out) %>% nrow()
      if (x_r + 1 <= max_x && test == 0) {
        x_r <- x_r + 1
        x_l <- x_l + 1
      }
    } else if (jet == "<") {
      test <- draw_shape(shapes[j], x_l - 1, x_r - 1, y) %>%
        intersect(out) %>% nrow()
      if (x_l - 1 >= min_x && test == 0) {
        x_l <- x_l - 1
        x_r <- x_r - 1
      }
    }
    i <- i + 1
    if (i > length(input)) i <- 1
    jet <- input[i]
    # fall 1 unit:
    y <- y - 1
    shape_coord <- draw_shape(shapes[j], x_l, x_r, y)
    # ready to settle?
    if (nrow(intersect(shape_coord, out)) > 0) {
      y <- y + 1
      shape_coord <- draw_shape(shapes[j], x_l, x_r, y)
      out <- rbind(shape_coord, out)
      #*
      m <- m + 1
      out2[[m]] <- shape_coord %>%
        mutate(shape = shapes[j], n = m, jet = i - 1)
      break
    }
  }
  j <- j + 1
  if (j > length(shapes)) j <- 1
}
out2 <- do.call(rbind, out2)
# a cycle of 3000?
x1 <- out2 %>%
  arrange(y) %>%
  filter(shape == 1) %>%
  nest(data = x) %>%
  mutate(
    data = data %>%
      map_chr(~ flatten_dbl(.x) %>% paste0(collapse = "")),
    y_diff = y - lag(y, default = 1)
  )
x1 %>%
  # filter(n > 1500) %>%
  group_by(data, y_diff, jet) %>%
  tally() %>%
  View()
x1 %>%
  mutate(x = if_else(y_diff == 5 & data == "1234" & jet == 1120, 1, 0)) %>%
  View()
# n (diff = 1745)
# 261 - 2006
# 186 - 1931
#
# y (diff = 2767)
# 419 - 3186
# 308 - 3075
#
# at shape 186, height is 308
n_cycles <- floor((1000000000000 - 186) / 1745)
# + the remainer:
last_shape <- (1000000000000 - 186) %% 1745 + 186 #1010
last_height <- out2 %>% filter(n == last_shape) %>% pull(y) %>% max()
308 + 2767 * n_cycles + (last_height - 308)
## 1585673352422 [correct]


#####
# x1 <- out2 %>%
#   arrange(x) %>%
#   filter(shape == 1) %>%
#   nest(data = x) %>%
#   rowwise() %>%
#   mutate(data = map(data, flatten_dbl)) %>%
x1 %>%
  filter(n > 1500) %>%
  group_by(data, y_diff)
3456
8

###
max(out$y)
# backup <- out
# # find repeating in out (~1:56?)
out2 <- out %>%
  filter(y > 0) %>%
  arrange(x) %>%
  nest(data = x) %>%
  mutate(data = map(data, flatten_dbl)) %>%
  arrange(y)

# interval <- 10
# seq(0, 100, interval)

x1 <- out2[10:20,] %>%
  mutate(x = map(data, flatten_dbl)) %>%
  pull(x)
x2 <- out2[30:40,] %>%
  mutate(x = map(data, flatten_dbl)) %>%
  pull(x)

1000000000000

## Notes: ####
# Visualise:
filter(out, y > 0) %>% rbind(shape_coord) %>% plot()
plot(filter(out, y > 0))
out %>%
  filter(y > 0 & y < 10) %>%
  plot()
# plot(out, pch = 15, cex = 5)

# Troubleshoot:
heights <- readLines("data/heights.txt") %>% as.numeric()
max(out$y) == heights[25]


#3032 # wrong
#3212 # wrong
#3233 # too high
#3421 # too high
#3459 # too high
#3089?

# the 24th/25th one lands wrong

# heights <- as.numeric(readClipboard())
"
####

.#.
###
.#.

..#
..#
###

#
#
#
#

##
##
"

## Part 2: ####
moves <- input
n_moves <- length(moves)
shapes <- list(
  hline = matrix(
    c(
      rep(0L, 4),
      0:3
    ),
    ncol = 2
  ),
  cross = matrix(
    c(
      -c(0L, 1L, 1L, 1L, 2L),
      c(1L, 0L, 1L, 2L, 1L)
    ),
    ncol = 2
  ),
  edge = matrix(
    c(
      -c(0L, 1L, 2L, 2L, 2L),
      c(2L, 2L, 0L, 1L, 2L)
    ),
    ncol = 2
  ),
  vline = matrix(
    c(
      -(0:3),
      rep(0L, 4)
    ),
    ncol = 2
  ),
  square = matrix(
    c(
      -rep(0:1, each = 2L),
      rep(0:1, 2)
    ),
    ncol = 2
  )
)
lefts <- lapply(
  shapes,
  function(s) {
    v <- tapply(s[, 2], s[, 1], min)
    matrix(
      c(
        as.integer(names(v)),
        v
      ),
      ncol = 2
    )
  }
)
rights <- lapply(
  shapes,
  function(s) {
    v <- tapply(s[, 2], s[, 1], max)
    matrix(
      c(
        as.integer(names(v)),
        v
      ),
      ncol = 2
    )
  }
)
downs <- lapply(
  shapes,
  function(s) {
    v <- tapply(s[, 1], s[, 2], min)
    matrix(
      c(
        v,
        as.integer(names(v))
      ),
      ncol = 2
    )
  }
)
shape_dims <- vapply(shapes, apply, integer(2), 2, max)
any_in_df <- function(df1, df2) {
  if (nrow(df1) == 0L || nrow(df2) == 0L)
    return(FALSE)
  s <- split(df1[, 2], df1[, 1])
  any(vapply(
    seq_along(s),
    function(n) {
      v <- as.integer(names(s)[n])
      any(is.element(s[[n]], df2[df2[, 1] == v, 2, drop = FALSE]))
    },
    logical(1)
  ))
}
next_shape <- function(tower, shape_index, move_index, height) {
  highest_settled <- if (nrow(tower) == 0L)
    0L
  else
    max(tower[, 1])
  shape <- shapes[[shape_index + 1L]]
  left <- lefts[[shape_index + 1L]]
  right <- rights[[shape_index + 1L]]
  down <- downs[[shape_index + 1L]]
  lowest_shape <- min(shape[, 1])
  pos <- c(highest_settled - lowest_shape + 4L, 3L)
  finished <- FALSE
  while (!finished) {
    test_area <- tower[
      tower[, 1] <= pos[1] & tower[, 1] >= pos[1] - 5L,
      ,
      drop = FALSE
    ]
    new_pos <- pos
    new_pos[2] <- switch(
      moves[move_index + 1L],
      "<" = new_pos[2] - 1L,
      ">" = new_pos[2] + 1L
    )
    new_check <- switch(
      moves[move_index + 1L],
      "<" = t(apply(left, 1, "+", new_pos)),
      ">" = t(apply(right, 1, "+", new_pos))
    )
    if (
      all(new_check[, 2] >= 1L & new_check[, 2] <= 7L) &&
      !any_in_df(new_check, test_area)
    )
      pos <- new_pos
    move_index <- (move_index + 1L) %% n_moves
    new_pos <- pos
    new_pos[1] <- new_pos[1] - 1L
    new_down <- t(apply(down, 1, "+", new_pos))
    if (
      any(new_down[, 1] <= 0L) ||
      any_in_df(new_down, test_area)
    )
      finished <- TRUE
    else
      pos <- new_pos
  }
  resting_positions <- t(apply(shape, 1, "+", pos))
  if (any_in_df(resting_positions, test_area))
    stop(paste(
      print(resting_positions),
      print(test_area)
    ))
  tower <- rbind(resting_positions, tower)
  row_counts <- table(tower[, 1])
  filled_rows <- which(row_counts == 7)
  if (length(filled_rows) > 0) {
    highest <- as.integer(names(row_counts))[max(filled_rows)]
    height <- height + highest
    tower[, 1] <- tower[, 1] - highest
    tower <- tower[tower[, 1] > 0, , drop = FALSE]
  }
  stopifnot(ncol(tower) == 2)
  list(
    tower = tower,
    shape_index = (shape_index + 1L) %% length(shapes),
    move_index = move_index,
    height = height
  )
}
update <- function(state, n) {
  cat(n, "\r")
  cache <- state$cache
  cache_indices <- state$cache_indices
  cache_heights <- state$cache_heights
  cached_state <- state[1:4]
  cached_settled <- cached_state[[1]]
  cached_settled[] <- as.integer(cached_settled)
  cache_index <- state$shape_index*n_moves + state$move_index + 1L
  cache_match <- match(list(cached_settled), cache[[cache_index]])
  if (!is.na(cache_match)) {
    tower <- cache[[cache_index]][[cache_match]]
    first_index <- cache_indices[[cache_index]][[cache_match]]
    first_height <- cache_heights[[cache_index]][[cache_match]]
    stopifnot(identical(cached_settled, tower))
    return(list(
      tower = cached_settled,
      iters = c(first_index, n),
      heights = c(first_height, state$height),
      cache = cache,
      cache_indices = cache_indices,
      cache_heights = cache_heights
    ))
  }

  new_state <- do.call(next_shape, state[1:4])
  cache[[cache_index]] <- c(
    cache[[cache_index]],
    list(cached_settled)
  )
  cache_indices[[cache_index]] <- c(cache_indices[[cache_index]], n)
  cache_heights[[cache_index]] <- c(cache_heights[[cache_index]], state$height)
  c(
    new_state,
    list(
      cache = cache,
      cache_indices = cache_indices,
      cache_heights = cache_heights
    )
  )
}
iter <- 0L
state <- list(
  tower = matrix(integer(), ncol = 2),
  shape_index = 0L,
  move_index = 0L,
  height = 0L,
  cache = rep(list(list()), n_moves*length(shapes)),
  cache_indices = rep(list(integer()), n_moves*length(shapes)),
  cache_heights = rep(list(integer()), n_moves*length(shapes))
)
while (iter < 2022L) {
  cat(iter, "\r")
  if (length(state) == 6)
    break
  state <- update(state, iter)
  iter <- iter + 1L
}
stopifnot(length(state) == 6) # assuming cycle occurs in part one
resolve_state <- function(state, iter) {
  cycle_length <- diff(state$iters)
  additional_iters <- iter - state$iters[2]
  additional_cycles <- additional_iters %/% cycle_length
  additional_steps <- additional_iters %% cycle_length
  final_cycle_height <- state$heights[2] + additional_cycles*diff(state$heights)
  step_location <- vapply(
    state$cache_indices,
    match,
    integer(1),
    x = state$iters[1] + additional_steps
  )
  step_lookup <- c(which(!is.na(step_location)), na.omit(unlist(step_location)))
  step_height <- state$cache_heights[[step_lookup[1]]][[step_lookup[2]]]
  final_height <- final_cycle_height + step_height - state$heights[1]
  step_tower <- state$cache[[step_lookup[1]]][[step_lookup[2]]]
  final_tower_height <- if (nrow(step_tower) == 0)
    0L
  else
    max(step_tower[, 1])
  final_height + final_tower_height
}
resolve_state(state, 1000000000000)

### Another solution: #####
wind <- if_else(head(input, -1) == "<", -1, 1)
wind_size <- length(wind)
n_tetris = 2022
# heuristic: we do not need to keep track of more than 100 rows
cave <- matrix(logical(7 * 100), ncol = 7)
cave[1, ] <- TRUE
tetrises <- list(
  #' ####
  cbind(
    c(3, 3, 3, 3),
    c(3, 4, 5, 6)
  ),
  #' .#.
  #' ###
  #' .#.
  cbind(
    c(4, 3, 4, 5, 4),
    c(3, 4, 4, 4, 5)
  ),
  #' ..#
  #' ..#
  #' ###
  cbind(
    c(3, 3, 3, 4, 5),
    c(3, 4, 5, 5, 5)
  ),
  #' #
  #' #
  #' #
  #' #
  cbind(
    c(3, 4, 5, 6),
    c(3, 3, 3, 3)
  ),
  #' ##
  #' ##
  cbind(
    c(3, 4, 3, 4),
    c(3, 3, 4, 4)
  )
)

print_cave <- function(cave) {
  rows <- apply(
    cave, 1,
    \(x) paste(if_else(if_else(x == 1, "@", "-")), collapse = "")
  )
  cat(paste(rev(rows), collapse = "\n"), "\n\n")
}

highest_rock_position <- function(cave) {
  max(which(apply(cave, 1, max) > 0))
}

move_with_wind <- function(cave, tetris, wind_direction) {
  would_be_tetris <- cbind(tetris[, 1], tetris[, 2] + wind_direction)
  if (min(would_be_tetris[, 2]) < 1 || max(would_be_tetris[, 2]) > 7) {
    return(tetris)
  }
  blocked_by_rock <- FALSE
  for (i in seq_len(nrow(would_be_tetris))) {
    if (cave[would_be_tetris[i, 1], would_be_tetris[i, 2]] != 0) {
      blocked_by_rock <- TRUE
    }
  }
  if (blocked_by_rock) return(tetris)
  tetris[, 2] <- tetris[, 2] + wind_direction
  tetris
}

is_blocked_from_below <- function(tetris) {
  is_blocked <- FALSE
  for (i in seq_len(nrow(tetris))) {
    if (cave[tetris[i, 1] - 1, tetris[i, 2]] != 0) {
      is_blocked <- TRUE
    }
  }
  is_blocked
}

move_down <- function(tetris) {
  tetris[, 1] <- tetris[, 1] - 1
  tetris
}

get_new_tetris <- function(cave, tetris_idx) {
  new_tetris <- tetrises[[tetris_idx]]
  new_tetris[, 1] <- new_tetris[, 1] + highest_rock_position(cave) + 1
  new_tetris
}

fill_cave_with_tetris <- function(cave, tetris) {
  for (i in seq_len(nrow(tetris))) {
    cave[tetris[i, 1], tetris[i, 2]] <- TRUE
  }
  cave
}

fallen_tetris_count <- 0
tetris_idx <- 1
wind_idx <- 1
current_tetris <- get_new_tetris(cave, tetris_idx)
max_heights <- integer(n_tetris)
result <- -1 # floor does not count

system.time({
  repeat {
    current_tetris <- move_with_wind(cave, current_tetris, wind[wind_idx])
    wind_idx <- wind_idx + 1
    if (wind_idx == wind_size + 1) wind_idx <- 1

    is_fallen <- is_blocked_from_below(current_tetris)
    if (is_fallen) {
      cave <- fill_cave_with_tetris(cave, current_tetris)
      fallen_tetris_count <- fallen_tetris_count + 1
      # heuristic that only the top few rows count
      highest <- highest_rock_position(cave)
      max_heights[fallen_tetris_count] <- result + highest
      if (highest >= 75) {
        result <- result + 25
        cave <- cave[-1:-25, ]
        cave[1, ] <- TRUE
        cave <- rbind(cave, matrix(logical(7 * 25), ncol = 7))
      }
      if (fallen_tetris_count == n_tetris) break
      tetris_idx <- tetris_idx + 1
      if (tetris_idx == 6) tetris_idx <- 1
      current_tetris <- get_new_tetris(cave, tetris_idx)
      # print_cave(fill_cave_with_tetris(cave, current_tetris))
    } else {
      current_tetris <- move_down(current_tetris)
    }
  }
})

# part 1: run with n_tetris = 2022
tail(max_heights, 1) #3212

##
#### Not mine: ####
# Parse Inputs ---
# instr <- strsplit(">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>", "")[[1]]
instr <- input
rocks5 <- readr::read_file("data/17.txt") %>%
  gsub("\r", "", .) %>%
  strsplit("\n\n") %>%
  purrr::pluck(1) %>%
  lapply(function(x) gsub("\n", "", x))

bool_rock <- function(rock_str, R, C){
  r <- strsplit(rock_str, "")[[1]] == '#'
  matrix(r, R, C, byrow = TRUE)
}
rocks <- list(
  # ---
  bool_rock(rocks5[[1]], 1, 4),
  # + symbol
  bool_rock(rocks5[[2]], 3, 3),
  # horizontal flipped L
  bool_rock(rocks5[[3]], 3, 3),
  # I
  bool_rock(rocks5[[4]], 4, 1),
  # square
  bool_rock(rocks5[[5]], 2, 2)
)

# Play tetris ---

grid_print <- function(mat, last_N){
  mat <- tail(mat, last_N)
  mat[mat] <- '#'
  mat[mat == "FALSE"] <- '.'
  for(i in seq_len(nrow(mat))){
    cat('|', paste0(mat[i,], collapse = ""), '|')
    cat('\n')
  }
  cat('+', paste(rep('-', 7),collapse = ""), '+', '\n')
}
move_rock <- function(dx, dy, rock){
  # remove rock from grid
  grid[cy, cx] <<- xor(grid[cy, cx], rock)

  # if nothing in way, move to new position
  if(!any(grid[cy+dy, cx+dx] & rock)){
    grid[cy+dy, cx+dx] <<- grid[cy+dy, cx+dx] | rock
    cy <<- cy + dy;  cx <<- cx + dx
    return(TRUE)
  } else {
    # else restore to original pos
    grid[cy, cx] <<- grid[cy, cx] | rock
    return(FALSE)
  }
}

top_N <- function(n = 30) as.vector(grid[last_h:pmin(total_h, last_h+n),])

grid <- matrix(FALSE, nrow = 1e5, ncol = 7)
cx <- cy <- 0

last_h <- total_h <- 1e5
r <- i <- jid <- rid <- 0 #counters for rock and jet
NI <- length(instr) # length of instructions
additional <- max_h <- 0

cache <- collections::dict()
# we need 3 things, structure at top (this can be a 2D boolean grid un-winded to
# vector of length N*7) rock id and jet id

while(r <= 1e12){
  if(r == 2022) print(max_h)
  # rotate between 5 rocks
  r <- r + 1; rid <- (r-1L) %% 5L + 1L
  srock <- rocks[[rid]]
  W <- ncol(srock); H <- nrow(srock)

  key <- list(rid, jid, top_N(30))
  if(cache$has(key)){
    tmp <- cache$get(key)
    nr <- tmp[1] # at which round was this last seen
    nh <- tmp[2] # what was the max height then
    d <- (1e12 - r) %/% (r - nr)
    m <- (1e12 - r) %%  (r - nr)
    r <- r + (r-nr) * d
    additional <- additional + d  * (max_h - nh)
    cache$clear()
  }
  cache$set(key, c(r, max_h))

  # note rock coordinates
  cx <<- 3:(3+W-1)
  cy <<- (last_h - 3 - H):(last_h - 4)

  # since we don't count grid bottom as floor, move one up
  if(r == 1) cy <<- cy + 1

  # place rock
  grid[cy, cx] <- grid[cy, cx] | srock

  # go through instructions until rock settles
  while(TRUE){
    i <- i + 1; jid <- (i-1L) %% NI + 1L
    wind <- instr[jid]

    if(wind == '>' & cx[W] < 7){
      move_rock(1L, 0L, srock)
    } else if(wind == '<' & cx[1] > 1){
      move_rock(-1L, 0L, srock)
    }

    # move down if no obstructions
    if(move <- cy[H] < total_h)
      move <- move_rock(0L, 1L, srock)

    if(!move) break

  }
  last_h <- min(cy[1], last_h)
  max_h <- total_h - last_h + 1
}

# Part 2
print(max_h - 1 + additional, digits = 22)
