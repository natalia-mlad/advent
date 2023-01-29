library(tidyverse)
library(adventdrob)
# x <- readClipboard()
x <- advent_input(8, 2022, parse = T)

## Part 1:
# length(str_split(x$x[1], "")[[1]]) #99
# length(paste0(letters, rep(1:4, each = 26)))
x <- x %>%
  separate(x, paste0(letters, rep(1:4, each = 26)), sep = "", convert = T) %>%
  select_if(~ !any(is.na(.)))

out <- 0
for(i in 2:(nrow(x) - 1)) {
  for (j in 2:(ncol(x) - 1)) {
    tree <- x[[i, j]]
    test <- all(
      tree <= max(x[1:(i - 1), j]),
      tree <= max(x[(i + 1):nrow(x), j]),
      tree <= max(x[i, 1:(j - 1)]),
      tree <= max(x[i, (j + 1):ncol(x)])
    )
    if (test) {out <- out + 1}
  }
}
nrow(x) * ncol(x) - out #1719
# the loop makes the code quite slow (~20s)...
# took about 35 min to solve but was straight forward

## Part 2:
out2 <- c()
for(i in 2:(nrow(x) - 1)) {
  for (j in 2:(ncol(x) - 1)) {
    tree <- x[[i, j]]
    # looking up:
    a <- x[1:(i - 1), j]
    s1 <- 0
    for (k in nrow(a):1) {
      s1 <- s1 + 1
      if (a[k, ] >= tree) {
        break
      }
    }
    # looking down:
    a <- x[(i + 1):nrow(x), j]
    s2 <- 0
    for (k in 1:nrow(a)) {
      s2 <- s2 + 1
      if (a[k,] >= tree) {
        break
      }
    }
    # looking left
    a <- x[i, 1:(j - 1)]
    s3 <- 0
    for (k in ncol(a):1) {
      s3 <- s3 + 1
      if (a[, k] >= tree) {
        break
      }
    }
    # looking right:
    a <- x[i, (j + 1):ncol(x)]
    s4 <- 0
    for (k in 1:ncol(a)) {
      s4 <- s4 + 1
      if (a[, k] >= tree) {
        break
      }
    }
    # scenic score:
    score <- s1 * s2 * s3 * s4
    out2 <- c(out2, score)
  }
}
max(out2) #590824
# loop time: ~24s
# took about 25 min but very straight forward & no hiccups

###
## Test Case ####
y <- readClipboard()
y <- tibble(x = y)
y <- y %>%
  separate(x, letters[1:6], sep = "", convert = T) %>%
  select(-a)
out <- 0
for(i in 1:nrow(y)) {
  for (j in 1:ncol(y)) {
    if(i == 1){next}
    else if(j == 1) {next}
    else if(i == nrow(y)) {next}
    else if(j == ncol(y)) {next}
    else{
      tree <- y[[i, j]]
      test <- all(
        tree <= max(y[1:(i - 1), j]),
        tree <= max(y[(i + 1):nrow(y), j]),
        tree <= max(y[i, 1:(j - 1)]),
        tree <= max(y[i, (j + 1):ncol(y)])
      )
      if (test) {
        print(y[[i, j]])
        out <- out + 1
      }
    }
  }
}
nrow(y) * ncol(y) - out

# The top-right 1 is not visible from any direction
# The center 3 is not visible from any direction
# In the bottom row, but the 3 and 4 are not.


out2 <- c()
for(i in 1:nrow(y)) {
  for (j in 1:ncol(y)) {
    if(i == 1){next}
    else if(j == 1) {next}
    else if(i == nrow(y)) {next}
    else if(j == ncol(y)) {next}
    else{
      tree <- y[[i, j]]

      # looking up:
      a <- y[1:(i - 1), j]
      s1 <- 0
      for (k in nrow(a):1) {
        s1 <- s1 + 1
        if (a[k, ] >= tree) {
          break
        }
      }

      # looking down:
      a <- y[(i + 1):nrow(y), j]
      s2 <- 0
      for (k in 1:nrow(a)) {
        s2 <- s2 + 1
        if (a[k, ] >= tree) {
          break
        }
      }

      # looking left
      a <- y[i, 1:(j - 1)]
      s3 <- 0
      for (k in ncol(a):1) {
        s3 <- s3 + 1
        if (a[, k] >= tree) {
          break
        }
      }

      # looking right:
      a <- y[i, (j + 1):ncol(y)]
      s4 <- 0
      for (k in 1:ncol(a)) {
        s4 <- s4 + 1
        if (a[, k] >= tree) {
          break
        }
      }

      # scenic score:
      score <- s1 * s2 * s3 * s4
      out2 <- c(out2, score)
    }
  }
}
max(out2)

# Another Way ####
highest_yet <- function(x) x > lag(cummax(x), default = -1)
x %>%
  grid_tidy(x) %>%
  group_by(row) %>%
  mutate(from_left = highest_yet(value),
         from_right = rev(highest_yet(rev(value))))
