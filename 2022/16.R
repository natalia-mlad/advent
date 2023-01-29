library(tidyverse)
library(adventdrob)
library(unglue)
library(tidygraph)
library(ggraph)
library(igraph)
input <- advent_input(16, 2022, parse = T)

## Function: ####
calc_path <- function(path, time = 30) {
  pressure <- 0
  my_graph <- my_graph %>%
    activate("nodes") %>%
    mutate(distance = node_distance_to(which(valves$name == "AA"), mode = "out"))
  for (i in 2:length(path)) {
    selected <- my_graph %>% filter(name == path[i])
    my_graph <- my_graph %>%
      mutate(distance = node_distance_to(which(valves$name == path[i]), mode = "out"))
    if(selected %>% pull(value) %>% .[1] == 0) next
    time <- time - (selected %>% pull(distance) %>% .[1] + 1)
    if(time <= 0) break
    pressure <- pressure + time * selected %>% pull(value) %>% .[1]
  }
  return(pressure)
}

## Part 1: ####
x <- unglue_data(
  input$x,
  "Valve {valve} has flow rate={rate}; {t} to {v} {next_valve}",
  convert = T
) %>%
  as_tibble() %>%
  select(-t, -v) %>%
  mutate(next_valve = str_split(next_valve, ", "))

my_graph <- x %>%
  select(valve, next_valve) %>%
  unnest(next_valve) %>%
  rename(from = valve, to = next_valve) %>%
  as_tbl_graph()
valves <- as_tibble(my_graph) %>%
  left_join(x, by = c("name" = "valve")) %>%
  select(name, rate)
good_valves <- valves %>% filter(rate > 0) %>% pull(name)
my_graph <- my_graph %>%
  activate("nodes") %>%
  mutate(value = valves$rate)

calc_path(c("AA", "MD", "DS", "YW", "SS", "FS", "HG", "PZ", "JE")) #1547
calc_path(c("AA", "YW", "MD", "DS", "SS", "FS", "HG", "PZ", "JE")) #1438

out <- expand.grid(rep(list(good_valves), 6), stringsAsFactors = F) %>%
  tibble() %>%
  rowwise() %>%
  mutate(n = luna(c(Var1, Var2, Var3, Var4, Var5, Var6))) %>%
  filter(n == 6)
# 3,603,600


## Test Case: ####
y <- readClipboard()
y <- unglue_data(
  y,
  "Valve {valve} has flow rate={rate}; {t} to {v} {next_valve}",
  convert = T
) %>%
  as_tibble() %>%
  select(-t, -v) %>%
  mutate(next_valve = str_split(next_valve, ", "))

y %>% filter(rate > 0)


## Notes: ####
plot(my_graph)
my_graph %>%
  ggraph() +
  geom_edge_link() +
  geom_node_point(size = 8, colour = 'steelblue') +
  geom_node_text(aes(label = name)) +
  theme_graph()

my_graph <- my_graph %>%
  activate("nodes") %>%
  mutate(value = valves$rate) %>%
  # mutate(value_acc = map_bfs_dbl("AA", value))
  mutate(distance = node_distance_to(which(valves$name == "AA"), mode = "out"))

out <- list()
nodes <- c("AA")
pressure <- 0
i <- 1
time <- 30
while(time > 0){
  my_graph <- my_graph %>%
    activate("nodes") %>%
    mutate(distance = node_distance_to(which(valves$name == nodes[i]), mode = "out"))

  selected <- my_graph %>%
    filter(name %in% good_valves) %>%
    filter(!name %in% nodes) %>%
    filter(distance > 0) %>%
    arrange(desc(value)) %>%
    arrange(distance)

  time <- time - (selected %>% pull(distance) %>% .[1] + 1)
  nodes <- c(nodes, selected %>% pull(name) %>% .[1])
  pressure <- pressure + time * selected %>% pull(value) %>% .[1]
  i <- i + 1
}
pressure #1547 #toolow
# "AA" "MD" "DS" "YW" "SS" "FS" "HG" "PZ" "JE"


first <- my_graph %>%
  filter(name %in% good_valves) %>%
  arrange(desc(value)) %>%
  arrange(distance) %>%
  pull(name) %>%
  .[1]

my_graph %>%
  activate("nodes") %>%
  mutate(value = valves$rate) %>%
  # mutate(value_acc = map_bfs_dbl("AA", value))
  mutate(distance = node_distance_to(which(valves$name == "MD"), mode = "out"))%>%
  filter(name %in% good_valves) %>%
  arrange(desc(value)) %>%
  arrange(distance) %>%
  filter(distance > 0)

out <- list()
for (i in 1:length(good_valves)) {
  paths <- shortest_paths(my_graph, "AA", good_valves[i])$vpath
  out[[i]] <- map(paths, names)[[1]]
}

for(i in 1:length(out)) {
  print(calc_path(out[[i]]))
}


###
### another solution ####
evaluate_path <- function(path){
  if(runif(1) < 1/1000) cat(path, "\n")
  time_opened <- cumsum(map2_dbl(head(path, -1), tail(path, -1), ~ dd[.x, .y]) + 1)
  sum((max_t - time_opened) * rate[path[-1]])
}

get_length <- function(path){
  sum(map2_dbl(head(path, -1), tail(path, -1), ~ dd[.x, .y])) + length(path) - 1
}

follow_path <- function(path) {
  if(get_length(path) >= max_t) return(0)
  pressure <- evaluate_path(path)
  if(part2) cache_path(path, pressure)
  max(pressure, map_dbl(setdiff(good_valves, path), ~ follow_path(c(path, .))))
}

cache_path <- function(path, pressure) {
  key = paste0(sort(path), collapse = "-")
  valveset_best[key] <<- max(pressure, coalesce(valveset_best[key], 0))
}

evaluate_combo <- function(idx1, idx2) {
  if(length(intersect(valveset[[idx1]], valveset[[idx2]])) > 1) return(0)
  valveset_best[idx1] + valveset_best[idx2]
}

###
rate <- x %>% pull(rate, name = valve)
good_valves <- names(rate)[rate > 0] %>% sort()
adj <- x %>%
  select(valve, next_valve) %>%
  unnest(next_valve) %>%
  as.matrix()

# make shortest distance matrix between good valves:
dd <- matrix(Inf, nrow = nrow(x), ncol = nrow(x), dimnames = list(pull(x, valve), pull(x, valve)))
dd[adj] <- 1
dd[diag(T, nrow(dd))] <- 0
while(any(dd == Inf)) {
  walk(1:nrow(dd), function(i) walk(1:ncol(dd), function(j) dd[i, j] <<- min(dd[i,] + dd[,j])))
}
dd

# Part 1:
part2 <- F
max_t <- 30
follow_path("AA")
# [1] 1580

# Part 2:
part2 <- T
max_t <- 26
valveset_best = c("AA" = 0)
follow_path("AA") # 1177

valveset <- str_split(names(valveset_best), "-")
combo_best <- 0
for(idx1 in 1:(length(valveset) - 1)){
  for(idx2 in (idx1+1):length(valveset)){
    combo_best <- max(combo_best, evaluate_combo(idx1, idx2))
  }
}
combo_best #2213


######
# AA CR IK PZ SQ KI
# AA CR IT IK SS
# AA CR IT SQ KI IK DS
# AA CR KI FS TX MD
# AA CR MD FS TX YW
# AA CR SQ IK KI YB
# AA CR TX MD IK
# AA CR YB IT IK FS HG
# AA CR YB KI JE DS
# AA DS FS HG CR JE
# AA DS FS IK CR JE YB
# AA DS FS IK SQ CR
# AA DS MD JE PZ
# AA DS MD TX FS JE PZ
# AA DS MD TX HG YB
# AA DS MD YW IK YB IT
# AA DS MD YW IT IK JE
# AA DS MD YW TX CR KI YB
# AA DS MD YW TX SS FS
# AA DS TX HG MD FS
# AA DS TX IK YB IT
# AA DS YW FS IK CR KI
# AA DS YW HG KI
# AA DS YW IK MD SS
# AA FS DS MD TX KI CR
# AA FS HG YB KI CR IK
# AA FS IK YW MD TX JE
# AA FS MD CR IK KI JE
# AA FS MD SQ KI CR JE
# AA FS MD SS CR IT YB
# AA FS TX JE IK SS
# AA FS TX JE IK YW SS
# AA FS TX JE IT IK MD
# AA FS TX JE PZ KI SQ
# AA FS TX PZ CR IK JE
# AA FS TX YB IT CR KI SQ
# AA FS YB JE CR KI
# AA FS YW MD DS PZ
# AA FS YW SQ CR YB
# AA FS YW SQ KI
# AA HG FS CR
# AA HG FS MD IK CR
# AA HG JE CR IT SQ
# AA HG TX MD FS IK
# AA IK CR KI TX
# AA IK DS YW FS HG PZ
# AA IK IT KI DS MD
# AA IK JE CR YB TX DS
# AA IK JE IT SQ YB KI
# AA IK JE MD CR KI YB
# AA IK JE PZ YB HG
# AA IK JE SQ YB MD
# AA IK JE SQ YW SS
# AA IK JE TX PZ SQ
# AA IK JE YW IT YB
# AA IK KI CR JE FS TX
# AA IK KI IT TX DS MD
# AA IK KI JE
# AA IK PZ HG FS TX YW
# AA IK YB DS
# AA IK YB SQ CR JE PZ
# AA IK YW SS CR KI SQ
# AA IT JE IK YB
# AA IT KI CR FS MD
# AA IT SQ YW TX
# AA IT YB SQ KI CR IK
# AA IT YB TX MD FS
# AA JE CR KI SQ IT YB
# AA JE CR KI YB IK HG
# AA JE CR MD IK
# AA JE IK CR SQ IT KI YB
# AA JE IK TX YW HG
# AA JE IK TX YW SS MD
# AA JE IT SQ IK YB
# AA KI CR JE IK MD FS
# AA KI CR YB YW IK
# AA KI IK FS YW MD
# AA KI IT YW SS TX
# AA KI SQ CR IK IT JE
# AA KI YB IT SQ CR JE
# AA MD CR YB IT KI JE
# AA MD FS DS CR KI IT
# AA MD FS PZ JE TX SS
# AA MD IK JE TX IT CR
# AA MD JE IK CR YW TX
# AA MD JE TX DS HG
# AA MD SS IT KI YB
# AA MD TX FS IK YB KI IT
# AA MD TX JE YB CR PZ
# AA MD TX JE YB KI CR IT IK
# AA MD TX SS YW KI IT
# AA MD TX YB IK IT CR KI
# AA MD TX YB IT IK YW
# AA MD YB SQ JE IK
# AA MD YW DS FS TX JE IK
# AA MD YW IK CR KI IT YB SQ
# AA MD YW JE PZ DS
# AA MD YW KI IK JE FS
# AA MD YW PZ JE YB SQ
# AA MD YW TX YB IT CR SQ
# AA PZ IK YB YW
# AA PZ MD TX YW
# AA SQ CR IT FS
# AA SQ IK YB MD
# AA SQ IK YW FS MD
# AA SQ KI YB IK JE
# AA SQ YB IK JE TX MD
# AA SQ YB IT CR KI JE
# AA SS DS MD FS YW TX
# AA SS DS SQ KI
# AA SS DS YW TX JE
# AA SS FS CR KI IK
# AA SS JE CR
# AA SS JE YW TX
# AA SS PZ HG JE
# AA SS TX FS MD YW JE
# AA SS YB JE IK TX
# AA SS YW MD TX JE YB IT CR
# AA TX CR IK MD PZ
# AA TX CR IT YB IK FS
# AA TX CR SQ IT
# AA TX DS SS JE
# AA TX FS CR JE KI IT
# AA TX FS MD IK JE IT CR
# AA TX HG YB CR
# AA TX IK KI MD SS
# AA TX IK KI SS MD
# AA TX IK MD KI CR
# AA TX JE CR IK YW MD SS
# AA TX JE PZ IK YB KI SQ
# AA TX JE YB IK SS MD
# AA TX KI IK IT YB CR
# AA TX KI SQ IK CR IT
# AA TX MD CR IK JE PZ HG
# AA TX MD DS IK CR IT
# AA TX MD YW IK CR SQ
# AA TX SQ YB IK IT
# AA TX SS KI IK CR
# AA TX SS YW IK YB IT KI
# AA TX YW YB MD
# AA YB IK CR JE TX HG
# AA YB IT IK KI CR
# AA YB JE FS
# AA YB JE TX HG MD
# AA YB JE TX IK DS
# AA YW CR IT YB JE SQ
# AA YW CR KI DS
# AA YW FS CR IT KI YB JE
# AA YW FS KI IK
# AA YW FS SS MD CR IT
# AA YW FS TX SQ IT
# AA YW FS YB CR IT KI
# AA YW HG FS SS JE
# AA YW HG FS TX JE IK KI
# AA YW HG TX MD IK
# AA YW IK IT JE YB KI
# AA YW JE IK KI DS
# AA YW KI IT CR JE FS
# AA YW KI IT CR TX
# AA YW KI YB JE FS
# AA YW MD CR KI SQ JE
# AA YW MD FS JE IK TX
# AA YW MD HG IK KI IT
# AA YW MD IK KI
# AA YW MD JE FS IK
# AA YW MD JE YB CR IT IK
# AA YW MD JE YB IT PZ
# AA YW MD TX JE CR KI YB SQ
# AA YW SQ CR SS
# AA YW SS FS MD TX CR
# AA YW SS JE IK YB SQ
# AA YW SS MD CR KI IT
# AA YW TX FS CR KI
# AA YW TX FS HG JE IK YB
# AA YW TX IK SQ YB CR
# AA YW TX JE PZ YB CR KI
# AA YW YB IT IK FS
