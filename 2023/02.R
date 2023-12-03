library(tidyverse)
library(adventdrob)
input <- advent_input(2, 2023, parse = T)

# Part 1: ####
result <- input %>%
  mutate(
    id = row_number(),
    x = str_remove_all(x, "Game \\d*: ")
  ) %>%
  separate_longer_delim(x, delim = ", ") %>%
  separate_longer_delim(x, delim = "; ") %>%
  separate_wider_delim(x, delim = " ", names = c("n", "colour")) %>%
  mutate(n = as.numeric(n)) %>%
  pivot_wider(
    names_from = colour,
    values_from = n,
    values_fn = ~ max(.x)
  )
result %>%
  filter(red <= 12, green <= 13, blue <= 14) %>%
  pull(id) %>%
  sum()

# Part 2: ####
result %>%
  mutate(power = red*green*blue) %>%
  pull(power) %>%
  sum
