library(tidyverse)
library(rdataviewer)
?is.vector
?is.atomic()

x <- c(1:10)

GetLastValue <- function(x) {
  if (length(x)) {
    x[[(length(x))]] 
  } else {
    x
  }
}
GetLastValue(x)

GetEvenElements <- function(x) {
  if (length(x)) {
   x[seq_along(x) %% 2 == 0]
  } else {
    x
  }
}
GetEvenElements(x)

DropLastValue <- function(x) {
  if (length(x)) {
    x[-length(x)]
  } else {
    x
  }
}
DropLastValue(x)

OnlyEven <- function(x) {
  if (length(x)) {
    x[x %% 2 == 0]
  } else {
    x
  }
}
OnlyEven(x)


x <- c(-1:1, Inf, -Inf, NaN, NA)
x[-which(x > 0)]
x[x <= 0]

x <- c(1:10)
x[11]

x <- hms::hms(3600)
class(x)

tibble(x = list(1:5), y = c(1:5))



# 21.3.5 Exercises --------------------------------------------------------

library(tidyverse)
output <- vector("double", ncol(mtcars))
for (i in seq_along(mtcars)) {
  output[[i]] <- mean(mtcars[[i]])
}

df <- mtcars

x <- vector("double", ncol(df))  # 1. output
for (i in seq_along(df)) {            # 2. sequence
  x[[i]] <- mean(df[[i]])      # 3. body
}
x1

output

output <- vector("list", ncol(nycflights13::flights)) 
for (i in seq_along(nycflights13::flights)) {
  output[[i]] <- typeof(nycflights13::flights[[i]])
}

output <- vector("list", ncol(iris))
for (i in seq_along(iris)) {
  output[i] <- unique(iris[i])
}


n <- 10
mu <- c(-10, 0, 10, 100)
output <- vector("list", length(mu)) 
for (i in seq_along(output)) {
  output[[i]] <- rnorm(n, mean = mu[i])
}
output

?str_c

out <- ""
for (x in letters) {
  out <- stringr::str_c(out, x)
}
out
str_c(letters, collapse = "")


humps <- c("five", "four", "three", "two", "one")
out <- vector("list", length(x)) 
for (i in seq_along(out)) {
  print("Alice has x[[i]] humps")
}
