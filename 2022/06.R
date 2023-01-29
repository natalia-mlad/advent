library(tidyverse)
library(adventdrob)
x <- advent_input(6, 2022, parse = T)
x <- str_split(x$x, "")[[1]]
# x <- str_split("bvwbjplbgvbhsrlpgdmjqwftvncz", "")[[1]]

# Part 1:
for (i in 1:length(x)) {
  out <- x[i:(i + 3)]
  if(length(unique(out)) == 4) {
    print((i + 3))
    break
  }
}
# 1909

# Part 2:
for (i in 1:length(x)) {
  out <- x[i:(i + 13)]
  if(length(unique(out)) == 14) {
    print((i + 13))
    break
  }
}
# 3380

## stats:
#  00:07:27   4169      0   00:08:24   3492      0
