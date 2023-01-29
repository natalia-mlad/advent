library(tidyverse)
library(adventdrob)

# x <- readClipboard()
x <- advent_input(2, 2022, parse = T)

# A = rock
# B = paper
# C = scissors
# X = rock (1)
# Y = paper (2)
# Z = scissors (3)

# 6 points win
# 0 points loss
# 3 points draw

x <- x %>%
  mutate(out = case_when(
    x == "A Y" ~ (2 + 6),
    x == "A X" ~ (1 + 3),
    x == "A Z" ~ (3 + 0),
    x == "B X" ~ (1 + 0),
    x == "B Y" ~ (2 + 3),
    x == "B Z" ~ (3 + 6),
    x == "C Z" ~ (3 + 3),
    x == "C X" ~ (1 + 6),
    x == "C Y" ~ (2 + 0)
  ))

summary(as.factor(x$out))
sum(x$out) #11103
# 17.5 min part 1

# X means you need to lose,
# Y means you need to end the round in a draw,
# and Z means you need to win"
x <- x %>%
  mutate(out2 = case_when(
    x == "A Z" ~ (2 + 6),
    x == "B Z" ~ (3 + 6),
    x == "C Z" ~ (1 + 6),
    x == "A Y" ~ (1 + 3),
    x == "B Y" ~ (2 + 3),
    x == "C Y" ~ (3 + 3),
    x == "A X" ~ (3 + 0),
    x == "B X" ~ (1 + 0),
    x == "C X" ~ (2 + 0)
  ))

sum(x$out2) #23.5min part 2

# A = rock
# B = paper
# C = scissors
# X = rock (1)
# Y = paper (2)
# Z = scissors (3)
