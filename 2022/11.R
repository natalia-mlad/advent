library(tidyverse)
library(adventdrob)
library(rlang)
x <- advent_input(11, 2022, parse = T)
# input <- x

## Part 1: ####
extract_digits <- function(x) {
  str_extract_all(x, "[:digit:]+") %>% as.numeric()
}
nm1 <- c("monkey", "start_items", "op", "test", "if_true", "if_false")

x <- split(x$x, cumsum(is.na(x))) %>%
  map(na.omit) %>%
  tibble(x = .) %>%
  unnest_wider(x, names_repair = ~ nm1) %>%
  suppressMessages() %>%
  mutate(
    monkey = extract_digits(monkey),
    start_items = str_extract_all(start_items, "[:digit:]+"),
    op = str_remove(op, "Operation: new = "),
    test = extract_digits(test),
    if_true = extract_digits(if_true),
    if_false = extract_digits(if_false)
  )
monkey_rules <- x %>% select(-start_items)
x <- x %>%
  select(monkey, start_items) %>%
  unnest(start_items) %>%
  mutate(start_items = as.numeric(start_items))

monkeys_count <- as_list(rep(0, nrow(monkey_rules)))

for (i in 1:20) {
  print(paste0("round ", i))
  for (k in 0:max(monkey_rules$monkey)) {
    curr_monkey <- x %>% filter(monkey == k)
    if (nrow(curr_monkey) == 0) next
    x <- x %>% filter(monkey != k)
    monkeys_count[[k + 1]] <- monkeys_count[[k + 1]] + nrow(curr_monkey)
    for (j in 1:nrow(curr_monkey)) {
      old <- curr_monkey$start_items[j]
      worry_lvl <- floor(eval(parse_expr(monkey_rules$op[k + 1])) / 3)
      if (worry_lvl %% monkey_rules$test[k + 1] == 0) {
        out <- tibble(monkey = monkey_rules$if_true[k + 1], start_items = worry_lvl)
      } else {
        out <- tibble(monkey = monkey_rules$if_false[k + 1], start_items = worry_lvl)
      }
      x <- x %>%
        full_join(out, by = c("monkey", "start_items")) %>%
        arrange(monkey)
    }
  }
}
# monkeys_count
# 5 238   9 231  11 239 235 246
monkeys_count %>%
  unlist %>%
  sort %>%
  tail(2) %>%
  prod
# 58794 # correct

## comments ####
#' Ahhh I'm so close to part two. I did a check for (new - divisor) * (new %/% divisor) ==/!= new but I'm still off :(. Can you explain you prime num multiplication?
#'                                                                                                                      Riinu Pius
#'                                                                                                                    @_Riinu_
#'                                                                                                                    ·
#'                                                                                                                    Dec 11
#'                                                                                                                    Replying to
#'                                                                                                                    @JiffyTweeting
#'                                                                                                                    I multiply (product) to get their least common multiple. Subtracting that from the new value won't change the test results. Instead of subtracting I keep the remainder in case multiples of that would fit in the number. Hope this helps 🙌
#' Doctor Jiffy
#' @JiffyTweeting
#' ·
#' Dec 11
#' Replying to
#' @_Riinu_
#' I think so? I'll give it a shot! One thing that I keep running into day after day is my lack of math shortcut knowledge 🫠
#'                                                                                                                    Doctor Jiffy
#'                                                                                                                    @JiffyTweeting
#'                                                                                                                    ·
#'                                                                                                                    Dec 11
#'                                                                                                                    Replying to
#'                                                                                                                    @JiffyTweeting
#'                                                                                                                    and
#'                                                                                                                    @_Riinu_
#'                                                                                                                    So it definitely works but my code is so egregious at this point its hard to track down where its still going wrong but thanks for the hint!
#'                                                                                                                      Brent Crossman
#'                                                                                                                    @brentcrossman
#'                                                                                                                    ·
#'                                                                                                                    Dec 11
#'                                                                                                                    Replying to
#'                                                                                                                    @_Riinu_
#'                                                                                                                    No idea why I thought I had to use LCM instead of just prod.... just over complicating things.

## Part 2: ####
# 20151213744


##
# notes ####
out <- c(1:4)
for(i in 1:length(out)) {
  print(i)
  if(out[i] %% 2 == 0) out <- c(out, 5)
  print(out)
}

out <- c(1:4)
i <- 1
while(i <= length(out)) {
  print(i)
  if(out[i] %% 2 == 0) out <- c(out, 5)
  i <- i + 1
}

##
y <- readClipboard()
y <- tibble(x = y) %>%
  mutate(x = str_trim(x))
y[y == ""] <- NA


#for(k in 0:max(x$monkey)) {
# max(x$monkey)


# map(start_items, ~ str_extract_all(.x, "[:digit:]+")))
# monkeys_count <- as.character(unique(x$monkey)) %>%
#   sapply(function(x) NULL)
# x <- x %>% mutate(to_remove = rep(0, nrow(x)))

### Someone elses solution ####
library(unglue)
# input_orig = read_csv("solutions/dayll/input", col_names = "all")
input_orig <- input
n_monkeys = 8
input_pattern = "Monkey {monkey}: Starting items: {old} Operation: new = old {op} {op_n} Test: divisible by {div} If true: throw to monkey {div1} If false: throw to monkey {div2}"

items_monkeys = na.omit(input_orig) %>%
  rename(all = x) %>%
  mutate(split = rep(1:n_monkeys, each = 6)) %>%
  group_by(split) %>%
  summarise(all = paste(all, collapse = " ")) %>%
  #slice(3) %>% pull(all)
  unglue_unnest(all, input_pattern, convert = TRUE) %>%
  separate_rows(old, sep = ", ", convert = TRUE) %>%
  mutate(
    monkey = monkey + 1,
    div1 = div1 + 1,
    div2 = div2 + 1,
    # op_n = as.numeric(op_n)
    op_n = parse_number(op_n)
  )
items = items_monkeys %>%
  select(monkey, old)
monkeys = items_monkeys %>%
  distinct(monkey, op, op_n, div, div1, div2)
monkey_handles = as.list(rep(0, nrow(monkeys)))
magic = prod(monkeys$div)

# Parts I and II
for (round in 1:10000) {
# for (round in 1:20) {
  if (round %% 100 == 0) {
    print(paste("round:", round))
  }
  for (mymonkey in unique(monkeys$monkey)) {
    #mymonkey = 3
    #print(paste( "monkey:", mymonkey))
    m_items = filter(items, monkey == mymonkey) %>%
      left_join(monkeys, by = "monkey")
    if (nrow(m_items) == 0) {
      next
    }
    rem_items = filter(items, monkey != mymonkey)
    monkey_handles[[mymonkey]] = monkey_handles[[mymonkey]] + nrow(m_items)

    items = m_items %>%
      mutate(op_n = if_else(is.na(op_n), as.numeric(old), op_n)) %>%
      rowwise() %>% # bc get() is not vectorised
      # Part I
      #mutate(new = floor(get(op)(old, op_n)) / 3) %>%
      #Part II
      mutate(new = get(op)(old, op_n)) %>%
      ungroup() %>%
      mutate(new = new %% magic) %>%
      mutate(monkey = if_else(new %% div == 0, div1, div2)) %>%
      select(monkey, old = new) %>%
      bind_rows(rem_items, .)
  }
}

monkey_handles %>%
  unlist() %>%
  sort() %>%
  tail(2) %>%
  prod()
# 58794
