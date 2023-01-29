library(tidyverse)
library(adventdrob)
input <- advent_input(19, 2022, parse = T)
input <- input %>%
  mutate(x = str_remove_all(x, "Blueprint [:digit:]+: ")) %>%
  separate(x, c("ore", "clay", "obs", "geode"), "[.] ") %>%
  mutate(
    ore = as.numeric(str_extract(ore, "[:digit:]+")),
    clay = as.numeric(str_extract(ore, "[:digit:]+")),
    obs = str_extract_all(obs, "[:digit:]+"),
    geode = str_extract_all(geode, "[:digit:]+"),
    obs_ore = map_dbl(obs, ~ .x[1] %>% as.numeric),
    obs_clay = map_dbl(obs, ~ .x[2] %>% as.numeric),
    geode_ore = map_dbl(geode, ~ .x[1] %>% as.numeric),
    geode_obs = map_dbl(geode, ~ .x[2] %>% as.numeric),
  ) %>%
  select(-obs, -geode)


## Part 1: ####
# ore -> ore robot
# ore -> clay robot
# ore + clay -> obs robot
# ore + obs -> geode robot
##
# you have exactly one ore-collecting robo
# Each robot can collect 1 of its resource type per minute.
# It also takes one minute for the robot factory
# (also conveniently from your pack) to construct any type of robot,
# although it consumes the necessary resources available when construction begins.
# which blueprint would maximize the number of opened geodes after 24 minutes by figuring out which robots to build and when to build them.
input %>%
  mutate(out = row_number())

# Example: ####
# 16th min
blueprint_efficiency <- function(ore, clay, obs_ore, obs_clay,
                                 geode_ore, geode_obs,
                                 obs_limit = 2,
                                 clay_limit = 3) {
  # set-up:
  ore_robots <- 1
  clay_robots <- 0
  obs_robots <- 0
  geode_robots <- 0

  build_clay <- FALSE
  build_geode <- FALSE
  build_obs <- FALSE

  n_ore <- 0
  n_clay <- 0
  n_obs <- 0
  n_geode <- 0

  # run:
  for(i in 1:24) {
    # print(n_ore)
    # delayed build:
    if (build_clay) {
      clay_robots <- clay_robots + 1
      build_clay <- FALSE
    }
    if (build_obs) {
      obs_robots <- obs_robots + 1
      build_obs <- FALSE
    }
    if (build_geode) {
      geode_robots <- geode_robots + 1
      build_geode <- FALSE
    }

    # build geode robot:
    if (n_obs >= geode_obs & n_ore >= geode_ore) {
      n_ore <- n_ore - geode_ore
      n_obs <- n_obs - geode_obs
      build_geode <- TRUE
    }
    # build obsidian robot:
    if (n_clay >= obs_clay & n_ore >= obs_ore & obs_robots < obs_limit) {
      n_ore <- n_ore - obs_ore
      n_clay <- n_clay - obs_clay
      build_obs <- TRUE
    }
    # build clay robot:
    if(n_ore >= clay & clay_robots < clay_limit) {
      n_ore <- n_ore - clay
      build_clay <- TRUE
    }
    if(n_ore >= clay & obs_robots > 0 & clay_robots < clay_limit + 1) {
      n_ore <- n_ore - clay
      build_clay <- TRUE
    }

    # collect:
    n_ore <- n_ore + ore_robots
    n_clay <- n_clay + clay_robots
    n_obs <- n_obs + obs_robots
    n_geode <- n_geode + geode_robots
  }

  return(n_geode)
}


n_ore
n_clay
n_obs

ore_robots
clay_robots
obs_robots

blueprint_efficiency(4, 2, 3, 14, 2, 7) #9
blueprint_efficiency(2, 3, 3, 8, 3, 12, obs_limit = 5, clay_limit = 5)

###
# Using blueprint 1 in the example above,
# the largest number of geodes you could open in 24 minutes is 9.
# Blueprint 1:
# Each ore robot costs 4 ore.
# Each clay robot costs 2 ore.
# Each obsidian robot costs 3 ore and 14 clay.
# Each geode robot costs 2 ore and 7 obsidian.
#
# Blueprint 2:
# Each ore robot costs 2 ore.
# Each clay robot costs 3 ore.
# Each obsidian robot costs 3 ore and 8 clay.
# Each geode robot costs 3 ore and 12 obsidian.
