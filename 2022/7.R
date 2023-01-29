library(tidyverse)
# x <- readClipboard()
x <- readLines("data/7.txt")

# Part 1: ####
## dir contents:
filesys_list <- function(x) {
  out <- list()
  current_dir <- c()
  for(i in 1:length(x)) {
    if (str_starts(x[i], "[$]")) {
      if (str_detect(x[i], "[$] cd [:alpha:]|/")) {
        current_dir <- c(current_dir, str_remove(x[i], "[$] cd "))
        dir_name <- paste0(current_dir, collapse = "/")
        out[[dir_name]] <- NULL
      } else if (x[i] == "$ cd ..") {
        current_dir <- head(current_dir, -1)
      }
      else if (x[i] == "$ ls") {
        z <- c()
      }
    } else {
      z <- c(z, x[i])
      out[[dir_name]] <- z
    }
  }
  return(out)
}
out <- filesys_list(x)

## files:
files <- out %>%
  map(~ keep(.x, ~ str_detect(.x, "[:digit:]"))) %>%
  compact() %>%
  map(~ str_extract(.x, "[:digit:]+") %>% as.numeric %>% sum)
files <- tibble(dir = names(files), file_size = flatten_dbl(files))

## finding the answer:
find_size <- function(my_dir) {
  files %>%
    filter(str_starts(dir, my_dir)) %>%
    pull(file_size) %>%
    sum()
}
out2 <- tibble(dir = names(out)) %>%
  mutate(size = map_dbl(dir, find_size))
out2 %>%
  # directories at most 100000:
  filter(size <= 100000) %>% #28 dirs
  pull(size) %>%
  sum
# 1491614
# took like at least an hour man :(

# Part 2: ####
unused_space <- 70000000 - out2[[1, "size"]]
unused_space > 30000000
space_needed <- 30000000 - unused_space
out2 %>%
  filter(size >= space_needed) %>%
  arrange(size) %>%
  head(1)
# 6400111
# less than 10 min

## Other Ways: ####
cd <- function(path, dir) {
  if (!is.na(dir)) {
    if (dir == "..") {
      return(head(path, -1))
    }
    return(c(path, paste0(tail(path, 1), "/", dir)))
  }
  return(path)
}

tibble(x) %>%
  extract(x, "cd_dir", "cd (.*)", remove = F) %>%
  mutate(path = c(accumulate(cd_dir, cd))) %>% #thats the key!
  unnest(path) %>%
  # head(20)
  filter(str_detect(x, "\\d")) %>% #find the files
  separate(x, c("size", "file"), sep = " ", convert = T) %>%
  group_by(path) %>%
  summarise(size = sum(size))
