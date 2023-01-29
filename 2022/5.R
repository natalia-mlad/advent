library(tidyverse)
library(adventdrob)
x <- advent_input(5, 2022, parse = F)

stack <- x[1:9, ]
procedure <- x[-c(1:10), ]

# Part 1:
## create stack_vectors:
stack2 <- stack %>%
  separate(x, c(letters, LETTERS), sep = ".*?") %>%
  t() %>%
  as_tibble() %>%
  mutate(V9 = as.numeric(V9)) %>%
  filter(!is.na(V9)) # %>% rowwise() %>% map(identity)
stack_vectors <- list()
for(i in 1:nrow(stack2)){
  stack_vectors[[i]] <- stack2[i, -c(9)] %>%
    as.character() %>%
    str_subset(" ", negate = T)
}

## fix procedure:
procedure <- procedure %>%
  separate(x, c(letters[1:6])) %>%
  select(b, d, f) %>%
  rename(n = b, from = d, to = f) %>%
  mutate_if(is.character, as.numeric)

## move crates:
move_crates <- function(n, from, to, out) {
  for (i in 1:n) {
    out[[to]] <- c(out[[from]][1], out[[to]])
    out[[from]] <- out[[from]][-1]
  }
  return(out)
}

out <- stack_vectors
for(i in 1:nrow(procedure)) {
  out <- move_crates(procedure$n[i], procedure$from[i], procedure$to[i], out)
}
out %>%
  lapply(`[[`, 1) %>%
  flatten_chr() %>%
  paste(collapse = "")


# Part 2:
move_crates2 <- function(n, from, to, out) {
  out[[to]] <- c(out[[from]][1:n], out[[to]])
  out[[from]] <- out[[from]][-c(1:n)]
  return(out)
}

out <- stack_vectors
for(i in 1:nrow(procedure)) {
  out <- move_crates2(procedure$n[i], procedure$from[i], procedure$to[i], out)
}
out %>%
  lapply(`[[`, 1) %>%
  flatten_chr() %>%
  paste(collapse = "")

# part 2 took just 3.5 min
