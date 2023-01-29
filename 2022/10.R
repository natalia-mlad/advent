library(tidyverse)
library(adventdrob)
x <- advent_input(10, 2022, parse = T)
# input <- x

## Functions: ####
count_cycles <- function(x){
  if(x == "noop") return(1) else return(2)
}

signal_strength <- function(data){
  new_data <- data %>%
    mutate(
      n = suppressWarnings(as.numeric(str_remove(x, "addx "))),
      cycle = cumsum(map_dbl(x, count_cycles)),
      during_cycle = cycle - 1,
    )

  tibble(during_cycle = 1:239) %>%
    full_join(new_data) %>%
    # left_join(tibble(cycle = 1:240)) %>%
    arrange(during_cycle) %>%
    mutate(
      n = replace_na(n, 0),
      val = lag(1 + cumsum(n), n = 1, default = 1),
      cycle = row_number(),
      signal = val * cycle
    )
}

# Part 1: ####
x <- signal_strength(x)
x %>%
  filter(cycle %in% (20 + 40 * (0:5))) %>%
  pull(signal) %>%
  sum()
# [1] 12560

# Part 2: ####
# x %>% fill(x, .direction = "up")
pos_sprite <- function(reg_x){
  if (reg_x > 0) reg_x <- reg_x - 1
  if (reg_x < 0) {
    out <- c("#", rep(".", 40 - 1))
  } else {
    out <- c(rep(".", reg_x), "###", rep(".", 40 - 3 - reg_x))
  }
  return(paste0(out, collapse = ""))
}
# range(x$val)

out <- c()
for(i in 1:nrow(x)){
  reg_x <- x$val[i]
  while(i > 40) i <- i - 40
  pixel <- reg_x %>%
    pos_sprite() %>%
    str_split("") %>%
    .[[1]] %>%
    .[i]
  out <- c(out, pixel)
}
out <- paste(out, collapse = "")
str_extract_all(out, '.{1,40}')[[1]] %>%
  tibble()
"PLPAFBCL"



######
x <- x %>%
  mutate(n = as.numeric(str_remove(x, "addx ")),
         n = replace_na(n, 0))

x <- x %>%
  mutate(
    cycle = cumsum(map_dbl(x, count_cycles)),
    during_cycle = cycle - 1,
    val = cumsum(n) + 1
  )
# x$during_cycle %>% max
x <- tibble(during_cycle = 1:240) %>%
  full_join(x) %>%
  fill(val) %>%
  mutate(signal = val * during_cycle)


x %>%
  filter(during_cycle == 20 | during_cycle == 60 | during_cycle == 100 |
           during_cycle == 140 | during_cycle == 180 | during_cycle == 220) %>%
  pull(signal) %>%
  sum()
# 13840 (too high)

# 20th, 60th, 100th, 140th, 180th, and 220th

## Test Case: ####
y <- readClipboard()
tibble(x = y) %>%
  signal_strength() %>%
  #filter(during_cycle > 215)
  filter(during_cycle %in% (20+40*(0:5)))

#select(during_cycle, cycle, x, val)
z <- tibble(x = y) %>% signal_strength()

out <- c()
for(i in 1:nrow(z)){
  reg_x <- z$val[i]
  while(i > 40) i <- i - 40
  pixel <- reg_x %>%
    pos_sprite() %>%
    str_split("") %>%
    .[[1]] %>%
    .[i]
  out <- c(out, pixel)
}
out <- paste(out, collapse = "")
str_extract_all(out, '.{1,40}')


#####
val <- 1
cycle <- 0
# for(i in 1:nrow(x)) {
#   cycle <- cycle + 1
#   if(x$x[i] == "noop") {
#     next
#   } else if()
#
# }

## Not mine: ####
input <- advent_input(10, 2022, parse = T)
processed <- input %>%
  separate(x, c("op", "num"), sep = " ", fill = "right", convert = T) %>%
  mutate(op_group = row_number(),
         num = coalesce(num, 0),
         cycles = ifelse(op == "addx", 2, 1)) %>%
  uncount(cycles) %>%
  group_by(op_group) %>%
  mutate(is_last = row_number() == n()) %>%
  ungroup() %>%
  mutate(x = cumsum(c(1, head(is_last * num, -1)))) %>%
  mutate(cycle = row_number())

processed %>%
  mutate(row = (cycle - 1) %/% 40,
         col = (cycle - 1) %% 40,
         pixel = ifelse(abs(col-x) <= 1, "#", ".")) %>%
  group_by(row) %>%
  summarise(paste0(pixel, collapse = ""))
