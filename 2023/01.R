library(tidyverse)
library(adventdrob)
input <- advent_input(1, 2023, parse = T)

# Part 1: ####
result <- input %>%
  mutate(
    y = map_chr(
      x,
      ~ str_extract_all(.x, "[:digit:]") %>%
        flatten_chr() %>%
        str_flatten()
    ),
    val = paste0(str_sub(y, 1, 1), str_sub(y, -1, -1)) %>%
      as.numeric()
  )
sum(result$val)

# Part 2: ####
library(textclean)
lookup <- tibble(number = 1:9, text = replace_number(1:9))

extract_numbers <- function(x) {
  processed <- x %>%
    str_extract_all(paste0(c("[:digit:]", lookup$text), collapse = "|")) %>%
    .[[1]]
  indices <- match(processed, lookup$text)
  processed[!is.na(indices)] <- lookup$number[indices[!is.na(indices)]]
  return(str_flatten(processed))
}

result <- input %>%
  mutate(
    x = str_replace_all(x, "twone", "twoone"),
    x = str_replace_all(x, "oneight", "oneeight"),
    x = str_replace_all(x, "threeight", "threeeight"),
    x = str_replace_all(x, "fiveight", "fiveeight"),
    x = str_replace_all(x, "nineight", "nineeight"),
    x = str_replace_all(x, "sevenine", "sevennine"),
    x = str_replace_all(x, "eightwo", "eighttwo"),
    x = str_replace_all(x, "eighthree", "eightthree"),
    y = map_chr(x, extract_numbers),
    val = paste0(str_sub(y, 1, 1), str_sub(y, -1, -1)) %>%
      as.numeric()
  )
sum(result$val)
