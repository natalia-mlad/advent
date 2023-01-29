library(tidyverse)


# Another way -------------------------------------------------------------
# devtools::install_github("dgrtwo/adventdrob", dependencies = T, build_vignettes = T, build_manual = T)
library(adventdrob)

x <- advent_input(1, 2022, parse = T)
out <- x %>%
  mutate(group = cumsum(is.na(x))) %>%
  count(group, wt = x) %>%
  arrange(desc(n))
out
out %>% head(3) %>% summarise(sum(n))

# Better Attempt ----------------------------------------------------------
x <- as.numeric(readLines("data/1.txt"))
out <- split(x, cumsum(is.na(x))) %>%
  map_dbl(~ sum(.x, na.rm = T))
max(out)
# 70509
out %>% sort %>% tail(3) %>% sum
# 208567

##
# Live attempt ------------------------------------------------------------
x <- readClipboard()
x <- as.numeric(x)

out <- c()
out2 <- c()
for (i in 1:length(x)) {
  if (is.na(x[i])) {
    next
  }
  out <- c(out, x[i])
  # print(x[i])
  # print(out)
  if (is.na(x[i + 1])) {
    # print(sum(out))
    out2 <- c(out2, sum(out))
    out <- c()
    # print(x[i])
  }
  # if(i == length(x)) {
  #   out2 <- c(out2, sum(out))
  # }
}
#242

## Part 1:
max(out2)

## Part 2:
out3 <- sort(out2, decreasing = T)
sum(out3[1:3])

# took me ~10min
