library(tidyverse)
library(adventdrob)
input <- advent_input(20, 2022, parse = T)

## Part 1: ####
decrypter <- function(y) {
  out <- y
  # between 40 and 100
  for (i in 1:length(y)) {
    # print(i)
    j <- match(y[i], out)
    k <- j + y[i]
    if (k > length(y)) {
      while(k > length(y)){
        k <- k - length(y) + 1
      }
      out[(k + 1):length(y)] <- out[-j][k:(length(y) - 1)]
    } else {
      while (k <= 1) {
        k <- length(y) - 1 + k
      }
      out[1:(k - 1)] <- out[-j][1:(k - 1)]
    }
    out[k] <- y[i]
    # print(out)
    if(length(unique(out)) != length(unique(y)))
      stop(paste0("Error at i=", i))
  }
  return(out)
}

out <- decrypter(input$x)


# we lostt -600 at i 19
out[-j][(k-1):(length(y) - 1)]



# Notes: ####
y <- readClipboard() %>%
  as.numeric()

decrypter(y)

out <- y
for(i in 1:length(y)) {
  j <- match(y[i], out)
  k <- j + y[i]
  if(k <= 1) k <- length(y) - 1 + k
  # out[1:(k - 1)] <- out[2:k]
  out[1:(k - 1)] <- out[-j][1:(k - 1)]
  out[k] <- y[i]
  # out[i] <- out[i + y[i]]
}
# also wasted too much time trying to match this exact behavior,
# but it turns out that it doesn't actually matter.
# 1, 2, -3, 0, 3, 4, -2 and -2, 1, 2, -3, 0, 3, 4
# are just two different ways of writing the same circular list
# (along with 5 other possible rotations).
