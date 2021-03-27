library(tidyverse)

library(lubridate)
library(nycflights13)
library(dplyr)
ymd(c("2010-10-10", "bananas"))

today("GMT")
?today()

d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014

mdy(d1)
ymd(d2)
dmy(d3)
mdy(d4)
mdy(d5)

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))


flights_dt %>%
  mutate(dep_hour = update(dep_time, yday = 1)) %>%
  mutate(sched_hour = update(sched_dep_time, yday = 1)) %>%
  mutate(delay = dep_hour - sched_hour) %>%
  mutate(month = factor(month(dep_time))) %>%
  ggplot(aes(dep_hour, color = month)) +
  geom_freqpoly(aes(y = ..density..), binwidth = 60 * 60)

flights_dt %>%
  mutate(dep_time_ = sched_dep_time + dep_delay * 60) %>%
  filter(dep_time_ != dep_time) %>%
  select(dep_time_, dep_time, sched_dep_time, dep_delay)

flights_dt %>%
  mutate(
    air_time_ = arr_time - dep_time,
    discrepancy = air_time - air_time_) %>%
  select(air_time_, air_time, arr_time, dep_time, origin, dest, discrepancy) %>%
  ggplot(aes(discrepancy)) +
  geom_freqpoly()

flights_dt %>%
  mutate(sched_dep_hour = hour(sched_dep_time)) %>%
  group_by(sched_dep_hour) %>% 
  summarise(dep_delay = mean(dep_delay)) %>%
  ggplot(aes(y = dep_delay, x = sched_dep_hour)) +
  geom_point() +
  geom_smooth()
  

flights_dt %>%
  mutate(sched_dep_wday = wday(sched_dep_time)) %>%
  mutate(sched_dep_wday = factor(wday(sched_dep_wday))) %>%
  group_by(sched_dep_wday) %>%
  summarise(dep_delay = mean(dep_delay)) %>%
  ggplot(aes(y = dep_delay, x = sched_dep_wday)) +
  geom_line() +
  geom_smooth()
  
flights_dt

how_old <- function(bday){
  ymd()
}
  
(today() %--% (today() + years(1))) / months(1)
rng[1]

### 19.2.1 Exercises ###

df <- tibble::tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

x <- c(df$a, NA)

rng <- range(x, na.rm = TRUE)
rng[]
(x - rng[1]) / (rng[2] - rng[1])

rescale01 <- function(x) {
  rng <- range(x, na.rm = FALSE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(x)

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(x)

?is.na


mean(is.na(x))

missing_mean <- function(x, y) {
  missing <- is.na(x)
  mean(y)
}

df <- tibble::tibble(
  x = c(1, 2, NA, NA, NA),
  y = c(5:9)
)

missing_mean(df$x, df$y)

x / sum(x, na.rm = TRUE)

x <- c(1:10)
percent_of_total <- function(x) {
  sum <- sum(x, na.rm = TRUE)
  x / sum
}
percent_of_total(x)

sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)

x <- c(1:10)

sd_over_mean <- function(x, na.rm = FALSE) {
  mean(x, na.rm = na.rm) / sd(x, na.rm = na.rm)
}
sd_over_mean(x)

x <- rnorm(10)
variance <- function(x, na.rm = TRUE) {
  n <- length(x)
  m <- mean(x)
  sq_err <- (x - m)^2
  sum(sq_err) / (n -1)
}
variance(x)

x <- c(1, 2, 5, 100)
skewness <- function(x, na.rm = TRUE) {
  n <- length(x)
  m <- mean(x)
  cube_err <- (x - m)^3
  numerator <- sum(cube_err) / (n -2)
  denominator <- (variance(x))^(3/2)
  numerator / denominator
}
skewness(x)

x <- c(NA, 3, 4, NA)
y <- c(NA, 2, 5, 6)

both_na <- function(x, y, na.rm = FALSE) {
  sum(is.na(x) & is.na(y))
}
both_na(x, y)


is_directory <- function(x) file.info(x)$isdir
is_readable <- function(x) file.access(x, 4) == 0

### The function is_directory() checks whether the path in x is a directory. 
### The function is_readable() checks whether the path in x is readable, 
### meaning that the file exists and the user has permission to open it. 
### These functions are useful even though they are short because 
### their names make it much clearer what the code is doing.


# 19.3.1 Exercises --------------------------------------------------------

?substr()

f1 <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}

f1("reply", "re")
has_prefix <- f1

f2 <- function(x) {
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
}

v1 <- c(1 , 2, 3)
f2(v1)
DeleteLastInteger
DropLast

f3 <- function(x, y) {
  rep(y, length.out = length(x))
}

?rep
x <- rnorm(5)
y <- rnorm(10)
f3(x, y)
# Repeats y once for each x
SubsetByX
Recycle


?MASS::mvrnorm()
?rnorm
?`if`
?`ifelse`

greet <- function(time = lubridate::now()) {
  hr <- lubridate::hour(time)
  if (hr < 12) {
    print("Good Morning")
  } else if (hr < 18) {
    print("Good Afternoon")
  } else {
    print("Good Evening")
  }
}

greet()



fizzbuzz <- function(x) {
  if (is.numeric(x) != TRUE) {
    print("Error")
  } else if (x %% 3 == 0 && x%%5 == 0) {
    print("fizzbuzz")
  } else if (x %% 3 == 0) {
    print("fizz")
  } else if (x %% 5 == 0) {
    print("buzz")
  } else {
      print(x)
    }
}


x <- "e"
switch(x, 
       a = ,
       b = "ab",
       c = ,
       d = "cd"
)

?switch()
?cut()

if (temp <= 0) {
  "freezing"
} else if (temp <= 10) {
  "cold"
} else if (temp <= 20) {
  "cool"
} else if (temp <= 30) {
  "warm"
} else {
  "hot"
}

fizzbuzz(c)


# 19.5.5 Exercises --------------------------------------------------------


letters <- letters[1:10]

commas <- function(...) stringr::str_c(..., collapse = ", ")

commas(letters)

rule("Title", pad = "-+")

rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}
rule("Important output", pad = "-+")

?paste0
?getOption

?mean()
?ceiling
?str_dup
?cat

show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  
  invisible(df)
}


# 20.3.5 Exercises --------------------------------------------------------

x <- c((0 / 0), (2/0))
is.finite(x)
dplyr::near
?abs()
?.Machine


ConvertDoubToInteger <- function(x, method) {
  if(method = "round down") {
    floor(x)
  } elseif(method = "round up") {
    ceiling(x)
  } else if(method = "round towards zero") {
    trunc(x)
  } elseif(method = "round away from zero") {
    sign(x) * ceiling(abs(x))
  } elseif(method = )
}