library(tidyverse)
library(adventdrob)
library(rlang)
input <- advent_input(21, 2022, parse = T)

## Part 1: ####
input <- input %>%
  mutate(
    monkey = str_extract(x, ".+(?=:)"),
    start = str_detect(x, "[:digit:]"),
    required = str_extract_all(x, "[:alpha:]+") %>% map(~ .x[-1]),
    x = str_replace(x, ":", " =")
  ) %>%
  arrange(desc(start))

for(i in 1:nrow(input)) {
  print(i)
  # do the easy first:
  if(input$start[i]) {
    eval(parse_expr(input$x[i]))
  } else {
    # check if all requirements are met:
    req <- input$required[[i]]
    my_env <- ls()
    # if all reqs are met then just execute:
    if(all(req %in% my_env)) {
      eval(parse_expr(input$x[i]))
    } else {
      # otherwise work through the reqs until you run out of unmet ones:
      todo <- req[!req %in% my_env]
      while(length(todo) != 0) {
        for (j in 1:length(todo)) {
          current <- input %>% filter(monkey == todo[j])
          req <- current$required[[1]]
          my_env <- ls()
          if(all(req %in% my_env)) {
            eval(parse_expr(current$x))
          } else {
            todo <- unique(c(todo, req))
          }
        }
        my_env <- ls()
        todo <- todo[!todo %in% my_env]
        # print(todo)
      }
    }
  }
}
root # 21120928600114

## Part 2: ####
# what number you need to yell so that root's equality check passes.
my_env <- ls()
rm(list = my_env[my_env != "input"])
input2 <- input %>% filter(monkey != "humn")

for(i in 1:nrow(input2)) {
  print(i)
  # do the easy first:
  if(input2$start[i]) {
    eval(parse_expr(input2$x[i]))
  } else {
    # check if all requirements are met:
    req <- input2$required[[i]]
    if(any(req == "humn")) next #*
    my_env <- ls()
    # if all reqs are met then just execute:
    if(all(req %in% my_env)) {
      eval(parse_expr(input2$x[i]))
    } else {
      # otherwise work through the reqs until you run out of unmet ones:
      todo <- req[!req %in% my_env]
      while(length(todo) != 0) {
        for (j in 1:length(todo)) {
          current <- input2 %>% filter(monkey == todo[j])
          req <- current$required[[1]]
          my_env <- ls()
          if(all(req %in% my_env)) {
            eval(parse_expr(current$x))
          } else {
            todo <- unique(c(todo, req))
          }
        }
        my_env <- ls()
        todo <- todo[!todo %in% my_env]
        if(any(todo == "humn")) break #*
        # todo <- todo[todo != "humn"] #*
        # print(todo)
      }
    }
  }
}

my_env <- ls()
quick_fix <- input %>%
  filter(monkey != "humn") %>%
  mutate(done = monkey %in% my_env) %>%
  filter(done == FALSE)
for(i in 1:nrow(quick_fix)) {
  my_env <- ls()
  if (all(quick_fix$required[[i]] %in% my_env)) {
    eval(parse_expr(quick_fix$x[i]))
  }
}
rm(i, j, req)

my_env <- ls()
input2 <- input2 %>%
  unnest(required) %>%
  mutate(done = required %in% my_env) %>%
  filter(done == FALSE) %>%
  mutate(
    eq = str_extract(x, "(?<=[=] ).+"),
    op = str_extract(eq, "[:punct:]|[+]"),
    eq = if_else(op %in% c("+", "-"), paste0("(", eq, ")"), eq)
  )

# rvrh == hzgl
# rvrh == 5697586809113
n = match("rvrh", input2$monkey)
out <- input2$x[n]
req <- input2$required[n]
# repeat this:
for(i in 1:nrow(input2)) {
  current <- input2 %>% filter(monkey == req)
  eq <- current %>% pull(eq)
  out <- sub(req, eq, out)
  req <- current$required
  if(req == "humn") break
}
out <- str_extract(out, "(?<=[=] ).+")

#3453*10^9:3454*10^9
tibble(humn = (3453748*10^6):(3453749*10^6)) %>%
  mutate(x = map_dbl(humn, ~ eval_tidy(parse_expr(out), data = list(humn = .x))) - hzgl) %>%
  filter(x == 0) %>%
  pull(humn)
# 3453748220116
