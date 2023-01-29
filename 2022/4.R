library(tidyverse)
library(adventdrob)
x <- advent_input(4, 2022, parse = T)

# Part 1:
out <- x %>%
  separate(x, c("x1min", "x1max", "x2min", "x2max")) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(out = case_when(
    x2min >= x1min & x2max <= x1max ~ 1,
    x2min <= x1min & x2max >= x1max ~ 1,
    TRUE ~ 0
  ))
sum(out$out) #524

# Part 2:
out2 <- 0
for(i in 1:nrow(out)) {
  x1 <- out[i, ]$x1min:out[i, ]$x1max
  x2 <- out[i, ]$x2min:out[i, ]$x2max
  if (length(intersect(x1, x2)) > 0) out2 <- out2 + 1
}
out2 #798


## Another Way: ####
out %>%
  rowwise() %>%
  mutate(out2 = sum(between(x1min:x1max, x2min, x2max))) %>%
  filter(out2 > 0) %>%
  nrow() #798
# mutate(out2 = list(x1min:x1max))



## NOTES: ####
# 00:09:20   4190      0   00:29:00   9357      0

# P2:
# contained <- function()
out %>%
  # mutate(out2 = list(x1min:x1max))
  mutate(out2 = case_when(
    out == 1 ~ 1,
    x2min <= x1min & x1max >= x2min ~ 1,
    TRUE ~ 0
  )) %>%
  pull(out2) %>%
  sum()
