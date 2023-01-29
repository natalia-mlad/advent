library(tidyverse)
library(adventdrob)
x <- advent_input(14, 2021, parse = T)

template <- x[1, ] %>% pull(x)
x <- x[-c(1:2), ]

# Part 1:
polymer <- function(x, y){
  x <- str_split(x, boundary("character"))[[1]]
  paste(c(x[1], y, x[2]), collapse = "")
}

out <- x %>%
  separate(x, c("x1", "x2"), remove = F) %>%
  mutate(out = map2_chr(x1, x2, polymer))

pair_insert <- function(input) {
  s <- str_detect(input, out$x1)
  pairs <- out %>% filter(s)
  # output <- str_replace_all(input, pairs$x1, pairs$out)
  output <- input
  for (i in 1:nrow(pairs)) {
    output <- str_replace_all(output, pairs$x1[i], pairs$out[i])
  }
  return(output)
}

n = 0
result <- template
repeat {
  n = n + 1
  result <- pair_insert(result)
  # print(n)
  if(n == 10) break
}
# result
# result <- pair_insert(pair_insert(pair_insert(pair_insert(pair_insert(pair_insert(pair_insert(pair_insert(pair_insert(pair_insert(template))))))))))
str_split(result, boundary("character"))[[1]] %>%
  as.factor() %>%
  summary() %>%
  range() %>%
  diff()
# 42806
# 3118

## NOTES: ####
str_replace_all(template, out$x1, out$out)

setequal(
  str_split("CHBHBKPHCPHPOKNSNCOVB", boundary("character"))[[1]],
  str_split("CHBBKPHCPHPOKNSNNCOVB", boundary("character"))[[1]]
)

length(str_split(template, boundary("character"))[[1]]) #20

str_split(template, boundary("character"), simplify = T)

pair_insert(template) %>%
  str_split(boundary("character"), simplify = T)
