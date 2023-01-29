library(tidyverse)
library(adventdrob)
x <- advent_input(1, 2016, parse = F)
x <- str_split(x$x, ", ")[[1]]
# x <- c("R8", "R4", "R4", "R8")

# Part 1:
# taxicab <- function(mode) {}
x_axis = 0
y_axis = 0
out <- tibble(x_axis, y_axis)
for(i in 1:length(x)) {
  step <- x[i]
  # TODO: There's a pattern here somewhere...
  if (i == 1 & str_detect(step, "^R")) {
    mode = "xaxis_plus"
    x_axis <- x_axis + as.numeric(str_remove_all(step, "R|L"))
  } else if (i == 1 & str_detect(step, "^L")) {
    mode = "xaxis_min"
    x_axis <- x_axis - as.numeric(str_remove_all(step, "R|L"))
  } else if (mode == "xaxis_plus" & str_detect(step, "^L")) {
    mode = "yaxis_plus"
    y_axis <- y_axis + as.numeric(str_remove_all(step, "R|L"))
  } else if (mode == "xaxis_plus" & str_detect(step, "^R")) {
    mode = "yaxis_min"
    y_axis <- y_axis - as.numeric(str_remove_all(step, "R|L"))
  } else if (mode == "xaxis_min" & str_detect(step, "^L")) {
    mode = "yaxis_min"
    y_axis <- y_axis - as.numeric(str_remove_all(step, "R|L"))
  } else if (mode == "xaxis_min" & str_detect(step, "^R")) {
    mode = "yaxis_plus"
    y_axis <- y_axis + as.numeric(str_remove_all(step, "R|L"))
  } else if (mode == "yaxis_plus" & str_detect(step, "^L")) {
    mode = "xaxis_min"
    x_axis <- x_axis - as.numeric(str_remove_all(step, "R|L"))
  } else if (mode == "yaxis_plus" & str_detect(step, "^R")) {
    mode = "xaxis_plus"
    x_axis <- x_axis + as.numeric(str_remove_all(step, "R|L"))
  } else if (mode == "yaxis_min" & str_detect(step, "^L")) {
    mode = "xaxis_plus"
    x_axis <- x_axis + as.numeric(str_remove_all(step, "R|L"))
  } else if (mode == "yaxis_min" & str_detect(step, "^R")) {
    mode = "xaxis_min"
    x_axis <- x_axis - as.numeric(str_remove_all(step, "R|L"))
  }
  # Location:
  # cat(paste0("(", x_axis, ", ", y_axis, ") "))
  out <- out %>% rbind(tibble(x_axis, y_axis))
}
abs(x_axis) + abs(y_axis) #250


# Part 2:
out2 <- matrix(ncol = 2)
for(i in 1:nrow(out)){
  if(i == nrow(out)) next
  out2 <- cbind(out$x_axis[i]:out$x_axis[i + 1], out$y_axis[i]:out$y_axis[i + 1]) %>%
    rbind(out2, .)
  out2 <- head(out2, -1)
}
out2 <- as_tibble(out2)
out2[duplicated(out2),] %>% slice(1) %>% abs() %>% sum() #151

# Fun plot:
out %>%
  ggplot(aes(x_axis, y_axis)) +
  geom_path()

###
## NOTES #####

# test cases:
x <- c("R2", "L3") #5
x <- c("R2", "R2", "R2") #2
x <- c("R5", "L5", "R5", "R3") #12

# Part 1:
x_axis = 0
y_axis = 0
for(i in 1:length(x)) {
  step <- x[i]
  # TODO: There's a pattern here somewhere...
  if (i == 1 & str_detect(step, "^R")) {
    # mode[1]
    mode = "xaxis_plus"
    x_axis <- x_axis + as.numeric(str_remove_all(step, "R|L"))
  } else if (i == 1 & str_detect(step, "^L")) {
    mode = "xaxis_min"
    x_axis <- x_axis - as.numeric(str_remove_all(step, "R|L"))
  } else if (mode == "xaxis_plus" & str_detect(step, "^L")) {
    mode = "yaxis_plus"
    y_axis <- y_axis + as.numeric(str_remove_all(step, "R|L"))
  } else if (mode == "xaxis_plus" & str_detect(step, "^R")) {
    mode = "yaxis_min"
    y_axis <- y_axis - as.numeric(str_remove_all(step, "R|L"))
  } else if (mode == "xaxis_min" & str_detect(step, "^L")) {
    mode = "yaxis_min"
    y_axis <- y_axis - as.numeric(str_remove_all(step, "R|L"))
  } else if (mode == "xaxis_min" & str_detect(step, "^R")) {
    mode = "yaxis_plus"
    y_axis <- y_axis + as.numeric(str_remove_all(step, "R|L"))
  } else if (mode == "yaxis_plus" & str_detect(step, "^L")) {
    mode = "xaxis_min"
    x_axis <- x_axis - as.numeric(str_remove_all(step, "R|L"))
  } else if (mode == "yaxis_plus" & str_detect(step, "^R")) {
    mode = "xaxis_plus"
    x_axis <- x_axis + as.numeric(str_remove_all(step, "R|L"))
  } else if (mode == "yaxis_min" & str_detect(step, "^L")) {
    mode = "xaxis_plus"
    x_axis <- x_axis + as.numeric(str_remove_all(step, "R|L"))
  } else if (mode == "yaxis_min" & str_detect(step, "^R")) {
    mode = "xaxis_min"
    x_axis <- x_axis - as.numeric(str_remove_all(step, "R|L"))
  }
}
# mode <- c("xaxis_plus", "xaxis_min",
#           "yaxis_plus", "yaxis_min")

abs(x_axis) + abs(y_axis) #250

# HabitReach95 <- approx(nls_line, df$days, xout = Asymp95)$y

x <- 1:10
y <- rnorm(10)
f <- approxfun(x, y)
f(x)

approx(0:8, rep(0, 9))
