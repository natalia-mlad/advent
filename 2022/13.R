library(tidyverse)
library(adventdrob)
input <- advent_input(13, 2022, parse = T) %>%
  filter(!is.na(x))

# Part 1:
x <- input %>%
  mutate(pair = rep(1:(nrow(input) / 2), each = 2))

out <- c()
for(i in seq(1, nrow(x), by = 2)) {
  print(i)
  left_p <- x[i,] %>% pull(x) %>% jsonlite::fromJSON()
  right_p <- x[i + 1,] %>% pull(x) %>% jsonlite::fromJSON()

  # compare intergers:
  if (is.integer(left_p) && is.integer(right_p)) {
    if (length(right_p) < length(left_p)) {
      next
    } else if (all(left_p <= right_p)) {
      out <- c(out, x[i,] %>% pull(pair))
    }

    # compare lists:
  } else if (is.list(left_p) && is.list(right_p)) {
    if (vec_depth(left_p) > vec_depth(right_p)) {
      next
    } else if (length(unlist(left_p)) < length(unlist(right_p))) {
      out <- c(out, x[i, ] %>% pull(pair))
    } else if (all(unlist(left_p) <= unlist(right_p))) {
      out <- c(out, x[i, ] %>% pull(pair))
    }

    # other types:
  } else if (is_empty(left_p)) {
    out <- c(out, y[i,] %>% pull(pair))
  }
}
sum(out)
# 5330
# correct:
#  [1]   3   5   6   9  10  11  12  18  22  25  28  30  31  33  35  36  38  39
# [19]  41  43  44  48  49  53  56  57  58  61  63  64  68  69  70  72  73  74
# [37]  75  77  84  85  86  88  89  90  92  93  95  97  99 102 108 109 115 116
# [55] 119 120 127 128 131 133 134 136 138 140 141 142 144 146 148 149
##
# incorrect:
#  [1]   3   4  11  12  13  15  16  17  21  22  23  27  30  33  34  35  36  38
# [19]  39  41  42  44  45  46  48  50  53  56  59  62  66  71  72  73  74  75
# [37]  77  80  81  83  84  85  88  89  93  94  98 100 102 104 107 108 109 114
# [55] 118 120 121 123 124 126 127 128 131 132 133 136 140 145 146 148 150
##
# setdiff(out, z)
#  [1]   4  13  15  16  17  21  23  27  34  42  45  46  50  59  62  66  71  80
# [19]  81  83  94  98 100 104 107 114 118 121 123 124 126 132 145 150
##

##
# Example: ####
y <- readClipboard()
y[y == ""] <- NA
y <- tibble(x = y) %>%
  filter(!is.na(x))
y <- y %>%
  mutate(pair = rep(1:(nrow(y) / 2), each = 2))


out <- c()
for(i in seq(1, nrow(y), by = 2)) {
  print(i)
  left_p <- y[i,] %>% pull(x) %>% jsonlite::fromJSON()
  right_p <- y[i + 1,] %>% pull(x) %>% jsonlite::fromJSON()

  # compare intergers:
  if (is.integer(left_p) && is.integer(right_p)) {
    if (length(right_p) < length(left_p)) {
      next
    } else if (all(left_p <= right_p)) {
      out <- c(out, y[i,] %>% pull(pair))
    }

  # compare lists:
  } else if (is.list(left_p) && is.list(right_p)) {
    if (vec_depth(left_p) > vec_depth(right_p)) {
      next
    } else {
      test <- c()
      for (j in 1:length(left_p)) {
        test <- c(test, all(unlist(left_p[[j]]) <= unlist(right_p[[j]])))
      }
      if (all(test)) {
        out <- c(out, y[i,] %>% pull(pair))
      }
    }

  # other types:
  } else if (is_empty(left_p)) {
    out <- c(out, y[i,] %>% pull(pair))
  }
}
sum(out)

# If both values are integers, the lower integer should come first. If the left integer is lower than the right integer, the inputs are in the right order. If the left integer is higher than the right integer, the inputs are not in the right order. Otherwise, the inputs are the same integer; continue checking the next part of the input.
# If both values are lists, compare the first value of each list, then the second value, and so on. If the left list runs out of items first, the inputs are in the right order. If the right list runs out of items first, the inputs are not in the right order. If the lists are the same length and no comparison makes a decision about the order, continue checking the next part of the input.
# If exactly one value is an integer, convert the integer to a list which contains that integer as its only value, then retry the comparison. For example, if comparing [0,0,0] and 2, convert the right value to [2] (a list containing 2); the result is then found by instead comparing [0,0,0] and [2].

### Another solution: ####
input <- advent_input(13, 2022)
# 1 right, -1 wrong, 0 check next
compare <- function(x, y){
  if(is.numeric(x) && is.numeric(y)){
    return(sign(y - x))
  }
  if(!is.list(x)) x <- list(x)
  if(!is.list(y)) y <- list(y)
  for(i in seq_len(length(x))){
    if(i > length(y)) return(-1) #right side ran out
    cmp <- compare(x[[i]], y[[i]]) # recurse
    if(cmp != 0) return(cmp)
  }
  if(length(x) == length(y)) return(0)
  return(1) # left side
}

packets <- input %>%
  mutate(x = str_replace_all(x, "\\[", "list("),
         x = str_replace_all(x, "\\]", ")"),
         pair = cumsum(x == "") + 1) %>%
  filter(x != "") %>%
  mutate(packet = map(x, ~ eval(parse(text = .))))

packets %>%
  group_by(pair) %>%
  summarise(packet1 = packet[1],
            packet2 = packet[2]) %>%
  mutate(cmp = map2_dbl(packet1, packet2, compare)) %>%
  filter(cmp >= 0) %>%
  summarise(sum(pair))
# 5330

packets %>%
  mutate(
    div1 = map_dbl(packet, compare, list(list(2))),
    div2 = map_dbl(packet, compare, list(list(6)))
  ) %>%
  summarise(
    position_divider_1 = sum(div1 == 1) + 1,
    position_divider_2 = sum(div2 == 1) + 2
  ) %>%
  mutate(position_divider_1 * position_divider_2)
# 27648

####
# file <- "data/13.txt"
# input <- trimws(readChar(file, file.info(file)$size))
# input <- strsplit(input, "\n\n")[[1]]
# input <- gsub("\\[", "list(", input)
# input <- gsub("\\]", ")", input)
# input <- gsub("^(.*)\\n(.*)$", "list(\\1, \\2)", input)
# input <- lapply(input, \(x) eval(str2lang(x)))
#
# is_ordered <- function(left, right) {
#   for (i in seq_len(max(length(left), length(right)))) {
#     if (i > length(left)) return(TRUE)
#     if (i > length(right)) return(FALSE)
#     if (is.list(left[[i]]) || is.list(right[[i]])) {
#       ordered <- is_ordered(as.list(left[[i]]), as.list(right[[i]]))
#       if(!is.na(ordered)) return(ordered)
#       next
#     }
#     if(left[[i]] < right[[i]]) return(TRUE)
#     if(left[[i]] > right[[i]]) return(FALSE)
#   }
#   NA
# }
#
# out <- c()
# for(i in seq(1, nrow(x), by = 2)) {
#   print(i)
#   left_p <- x[i,] %>% pull(x) %>% jsonlite::fromJSON()
#   right_p <- x[i + 1,] %>% pull(x) %>% jsonlite::fromJSON()
#   out <- c(out, is_ordered(left = left_p, right = right_p))
# }
