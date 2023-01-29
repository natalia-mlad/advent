library(tidyverse)
library(adventdrob)
input <- advent_input(3, 2022, parse = T)

###
common_item <- function(x, y) {
  x2 <- str_split(x, boundary("character"))[[1]]
  y2 <- str_split(y, boundary("character"))[[1]]
  out <- x2[x2 %in% y2] #intersect?
  return(unique(out))
}

priority <- tibble(out = c(letters, LETTERS), priority = c(1:52))

out <- input %>%
  mutate(
    n = str_length(x) / 2,
    comp1 = str_sub(x, 1, n),
    comp2 = str_sub(x, (n + 1), length(x))
  ) %>%
  mutate(out = map2_chr(comp1, comp2, common_item)) %>%
  left_join(priority)

# Part 1:
sum(out$priority)
# took about 20min

# Part 2:
common_item2 <- function(x, y, z) {
  x2 <- str_split(x, boundary("character"))[[1]]
  y2 <- str_split(y, boundary("character"))[[1]]
  z2 <- str_split(z, boundary("character"))[[1]]
  out1 <- x2[x2 %in% y2]
  out2 <- z2[z2 %in% out1]
  return(unique(out2))
}

nrow(input) / 3 #100
out2 <- input %>%
  mutate(n = rep(1:3, 100)) %>%
  pivot_wider(
    values_from = x,
    names_from = n,
    names_prefix = "n",
    values_fn = list
  ) %>%
  unnest(c(n1, n2, n3)) %>%
  mutate(out = pmap_chr(list(n1, n2, n3), common_item2)) %>%
  left_join(priority)

sum(out2$priority)
# took about 15 min

#12:56
#13:16 (part 2 start with small interuption)
#13:32
