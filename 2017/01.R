library(tidyverse)
library(adventdrob)
x <- advent_input(1, 2017, parse = F)
x <- str_split(x, boundary("character"))[[1]]

# Part 1:
out <- c()
for (i in 1:length(x)) {
  if (i == length(x)) {
    if (x[1] == x[length(x)]) {
      out <- c(out, x[i])
    }
  } else if (x[i] == x[i + 1]) {
    out <- c(out, x[i])
  }
}
sum(as.numeric(out)) #1097

# Part 2:
n <- length(x) / 2
out2 <- c()
for (i in 1:length(x)) {
  if (i + n > length(x)) {
    if (x[i] == x[i + n - length(x)]) {
      out2 <- c(out2, x[i])
    }
  } else if (x[i] == x[i + n]) {
    out2 <- c(out2, x[i])
  }
}
sum(as.numeric(out2)) #1188
