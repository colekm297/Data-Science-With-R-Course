install.packages("tidyverse")
install.packages("ggplot2")
install.packages("gapminder", "Lahman")

library(tidyverse)
library("nycflights13")
library("gapminder")
library("Lahman")
library("ggplot2")

mpg <- data.frame(ggplot2::mpg)

### Plot fuel use with engine size ###
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), shape = 0)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = manufacturer, y = displ, color = displ > 5))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ cyl)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2, ncol = 4)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = drv), size = 3) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv, linetype = drv), se = FALSE, size = 2)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, fill = drv)) +
  geom_point(shape = 21, color = "white", size = 4)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, fill = drv, color = drv)) +
  geom_point()

### WORKING WITH DIAMONDS DATA SET ###

diamonds <- data.frame(ggplot2::diamonds)

### FIND OUT WHICH STATISTIC IS ASSOCIATED WITH WHICH GEOM ####
?stat_
?stat_bin
?stat_smooth
?geom_count
?geom_jitter

### Adding a smooth line to help with interpretation)
ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = depth)) +
  geom_smooth(mapping = aes(x = carat, y = depth))
    
### Using Jitter function to randomize points for better display of data ###
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point() +
  geom_jitter(width = 5, height = 2)
  
### Use geom_count to make size of points based in count of points in each position ###
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_count()

ggplot(data = mpg, mapping = aes(x = drv, y = hwy)) + 
  geom_boxplot()

### Turning Stacked Bar Chart into a Pie Chart ###

bar <- ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = color, fill = color),
    show.legend = FALSE)
    
bar + coord_polar()

### What does Labs() do? ###
?labs

### what are fixed coordinates and abline lines? ###
?geom_abline #reference line

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline() 


### Workflow Tips ###
seq()
 
### Tweak Code to make Work ###

### Broken ###
library(tidyverse)

ggplot(dota = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

fliter(mpg, cyl = 8)
filter(diamond, carat > 3)

### Fixed ###

library(tidyverse)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

filter(mpg, cyl = 8)
filter(diamonds, carat > 3)

### Shortcut for ALL SHORTCUTS ###
##### SHIFT-ALT-K ######


########### TRANSFORMATIONS SECTION ################

library(nycflights13)
library(tidyverse)

nycflights <- data.frame(nycflights13::flights)

### Find all flights with an arrival delay of over two hours or more ###

filter(nycflights, arr_delay >= 120)

### Flew to Houston (IAH or HOU) ###

filter(nycflights, dest == "IAH" | dest == "HOU")

### Operated by United UA, Delta DL, or American AA ###

filter(nycflights, carrier == "UA" | carrier == "AA" | carrier == "DL")

filter(nycflights, carrier %in% c("UA", "AA", "DL"))

### Departed in Summer, July 7, August 8, or September 9 ###

filter(nycflights, month %in% c(7,8,9))

### Arrived more than two hours late, but left on time ###

filter(nycflights, arr_delay > 120 & dep_delay <= 0)

### Delayed departure by at least an hour, but made up at least 30 minutes in flight ###

Delay2 <- filter(nycflights, dep_delay >= 60 & arr_delay <= -30)

?between()

### Departed between midnight and 6am (inclusive) ###

filter(flights, dep_time == 2400 | dep_time <= 600)

### How many flights have a missing dep_time? ###

filter(nycflights, is.na(dep_time))

### Use Arrange function to to sort NA values to start of dep_delay ###

arrangeflights <- arrange(nycflights, desc(is.na(dep_delay)))

### Sort flights to find most delayed flights ###

arrangeflights2 <- arrange(nycflights, desc(dep_delay))

### Find highest speed flights distance / air_time ###

flightspeed <- nycflights$distance / nycflights$air_time
arrange(nycflights, desc(flightspeed))

### What happens if you call a variable multiple times in Select function ###

select(nycflights, distance)
?one_of()

### How to use vars vector in conjunction with one_of function ###

vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(nycflights, one_of(vars))

### How can you change default select function ###
select(flights, matches("TIME", ignore.case = FALSE))
?select()

flights_sml <- select(nycflights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)

### Convert dep_time and sched_dep_time to time since in minutes since midnight ###

small <- mutate(nycflights,
       dep_minutes = (dep_time%/%100)*60 + (dep_time%%100),
       sched_dep_minutes = (sched_dep_time%/%100)*60 + (sched_dep_time%%100)
       )

### Find 10 most delayed flights using min_rank ###

mostdelayed <- nycflights$dep_delay
delayed <- min_rank(desc(mostdelayed))
select(delayed, 1:10)
view(mostdelayed)

flights %>%
  arrange(flights, min_rank(desc(dep_delay))) %>%
  filter(flights, delayed <= 10)


by_dest <- group_by(nycflights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20, dest != "HNL")

?min_rank

#### Come up with another approach that will give you the same output as 
#### not_cancelled %>% count(dest) and not_cancelled %>% count(tailnum, wt = distance) 
#### (without using count()).

not_cancelled <- nycflights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% count(dest)

(by_dest <- not_cancelled %>% 
  group_by(dest) %>%
  summarise(n()))

not_cancelled %>% count(tailnum, wt = distance)

(by_tailnum <- not_cancelled %>% 
  group_by(tailnum) %>%
  summarise(sum(distance)))

### Look at the number of cancelled flights per day. 
### Is there a pattern? Is the proportion of cancelled flights related to the average delay?

(count_cancel <- nycflights %>%
  group_by(year, month, day) %>%
  summarise(
    cancel = sum(is.na(dep_delay)),
    not_cancel = sum(!is.na(dep_delay)),
    prop_cancel = (cancel / not_cancel),
    avg_delay = mean(dep_delay, na.rm = TRUE)))

ggplot(data = count_cancel, mapping = aes(x = prop_cancel, y = avg_delay)) +
  geom_point() +
  geom_smooth(se = FALSE)

### Which carrier has the worst delays? 
### Challenge: can you disentangle the effects of bad airports vs. bad carriers? 
### Why/why not? (Hint: think about flights %>% group_by(carrier, dest) %>% summarise(n()))

not_cancelled <- nycflights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

flights %>% group_by(carrier, dest) %>% summarise(n())

x <- not_cancelled %>%
  group_by(carrier) %>%
  summarise(delay = mean(dep_delay)) %>%
  arrange((desc(delay))) %>%
  slice(1)

### Which plane (tailnum) has the worst on-time record?

posdelay <- not_cancelled %>%
  group_by(tailnum) %>%
  filter(dep_delay > 0) %>%
  filter(arr_delay > 0)


worst_ontime <- not_cancelled %>% 
  group_by(tailnum) %>%
  summarise(arr_delay = mean(arr_delay), n = n()) %>%
  filter(n >= 20) %>%
  filter(min_rank(desc(arr_delay)) == 1)


### What time of day should you fly if you want to avoid delays as much as possible?

best_time <- not_cancelled %>%
  group_by(hour) %>%
  summarise(arr_delay = mean(arr_delay)) %>%
  arrange(arr_delay)

### For each destination, compute the total minutes of delay. 
### For each flight, compute the proportion of the total delay for its destination.

x <- flights %>%
  filter(arr_delay > 0) %>%
  group_by(dest) %>%
  mutate(
    totaldelay = sum(arr_delay),
    propdelay = arr_delay / totaldelay
         )

y <- x %>%
  group_by(flight) %>%
  summarise(delaybyflight = sum(propdelay))


x <- flights %>%
  filter(arr_delay > 0) %>%
  group_by(dest) %>%
  mutate(
    arr_delay_total = sum(arr_delay),
    arr_delay_prop = arr_delay / arr_delay_total
  ) %>%
  select(
    dest, month, day, dep_time, carrier, flight,
    arr_delay, arr_delay_prop
  ) %>%
  arrange(dest, desc(arr_delay_prop))
  
### Delays are typically temporally correlated: 
### even once the problem that caused the initial delay has been resolved, 
### later flights are delayed to allow earlier flights to leave. 
### Using lag(), explore how the delay of a flight 
### is related to the delay of the immediately preceding flight.

?lag()

lagged_delays <- flights %>%
  arrange(origin, month, day, dep_time) %>%
  group_by(origin) %>%
  mutate(dep_delay_lag = lag(dep_delay)) %>%
  filter(!is.na(dep_delay), !is.na(dep_delay_lag))

### Look at each destination. Can you find flights that are suspiciously fast? 
### (i.e. flights that represent a potential data entry error). 
### Compute the air time of a flight relative to the quickest flight to that destination. 
### Which flights were most delayed in the air?

x <- flights %>%
  filter(!is.na(air_time)) %>%
  group_by(dest, origin) %>%
  mutate(
    speed = distance / (air_time / 60),
    speedSD = sd(speed),
    avgspeed = mean(speed),
    stdspeed = (speed - avgspeed) / (speedSD + 1)
  ) %>%
  arrange(desc(stdspeed))

ggplot(x, mapping = aes(x = speed)) +
  geom_histogram()

### Find all destinations that are flown by at least two carriers. 
### Use that information to rank the carriers.

## MY code
x <- flights %>%
  filter(!is.na(air_time)) %>%
  group_by(dest) %>%
  mutate(n_carriers = n_distinct(carrier)) %>%
  filter(n_carriers > 1) %>%
  group_by(carrier) %>%
  summarise(n_dest = n_distinct(dest)) %>%
  arrange(desc(n_dest))
  

## their code
flights %>%
  # find all airports with > 1 carrier
  group_by(dest) %>%
  mutate(n_carriers = n_distinct(carrier)) %>%
  filter(n_carriers > 1) %>%
  # rank carriers by numer of destinations
  group_by(carrier) %>%
  summarize(n_dest = n_distinct(dest)) %>%
  arrange(desc(n_dest))



################ EXPLORATORY DATA ANALYSIS: CHAPTER 7 ####################


### Explore the distribution of each of the x, y, and z variables in diamonds. 
### What do you learn? 
### Think about a diamond and how you might decide which dimension 
### is the length, width, and depth.

smaller <- diamonds %>%
  filter(x >3)

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = x), binwidth = 0.1) +
  coord_cartesian(ylim = c(0,50))

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = x, y = y), alpha = .1) +
  coord_cartesian(xlim = c(3, 12), ylim = c(3,12))

(unusual <- diamonds %>%
  filter(x > 10 | x < 3) %>%
  select(price, x, y, z))

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0,50))

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = z), binwidth = 0.1)

### Explore the distribution of price. 
### Do you discover anything unusual or surprising? 
### (Hint: Carefully think about the binwidth and make sure you try a wide range of values.)

### Much more variation in price past a certain point (right skew)

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = price), binwidth = 10) 

### There is a gap at $1500 where no price is in that range. ###

ggplot(data = diamonds) +
  geom_freqpoly(mapping = aes(x = price), binwidth = 100) +
  coord_cartesian(xlim = c(0, 5000))

### Most common price is ~$750

ggplot(data = diamonds) +
  geom_freqpoly(mapping = aes(x = price), binwidth = 100) +
  coord_cartesian(xlim = c(0,2500))

### How many diamonds are 0.99 carat? 
### How many are 1 carat? 
### What do you think is the cause of the difference?

# Close to zero diamonds are .99 carat
# Around 1500 are carat of 1
# Marketing and cutting a 1 carat diamond is probably much easier

ggplot(data = diamonds) +
  geom_freqpoly(mapping = aes(x = carat), binwidth = .001) +
  coord_cartesian(xlim = c(0.98,1.00))

### Compare and contrast coord_cartesian() vs xlim() or ylim() 
### when zooming in on a histogram. 
### What happens if you leave binwidth unset?
### What happens if you try and zoom so only half a bar shows?

# Gets rid of all values outside of coordinates
# Picks a default binwidth of 30
# Data disappears
ggplot(data = diamonds) +
  geom_freqpoly(mapping = aes(x = carat)) +
  xlim(0.95,1.05)

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat)) +
  xlim(0.99,1.00)

### What happens to missing values in a histogram? 
### What happens to missing values in a bar chart? 
### Why is there a difference?

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = x), binwidth = .5, na.rm = TRUE)

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = x), na.rm = TRUE)

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = x), binwidth = .05, na.rm = TRUE) +
  xlim(0,3)

mean(diamonds$y, na.rm = TRUE)
?mean()

### Use what you’ve learned to improve the visualisation of the departure times 
### of cancelled vs. non-cancelled flights.

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(x = sched_dep_time, y = sched_dep_time)) + 
  geom_boxplot(mapping = aes(x = cancelled, y = sched_dep_time))

ggplot(data = flights) +
  geom_boxplot(mapping = aes(x = cancelled, y = dep_time))

### What variable in the diamonds dataset is most important 
### for predicting the price of a diamond? 
### How is that variable correlated with cut? 
### Why does the combination of those two relationships 
### lead to lower quality diamonds being more expensive?

# Carat
# Fair has the highest average carat, followed by premium
# You can make larger diamonds with cheaper cuts, it would be prohibitively expensive 
# were you to do that with more high-priced cuts

breaks <- c(0, 0.2, 0.4, 0.6, 0.8, 1, 5.01)
tags <- c("[0,.2)", "[.2,.4)", "[.4,.6)", "[.6,.8)", "[.8,1)", "[1,5.01)")
carat_bins <- cut(diamonds$carat, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags)
summary(carat_bins)

ggplot(data = diamonds, mapping = aes(x = carat_bins, y = price)) +
  geom_boxplot()

ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_bar()
?geom_bar()

ggplot(data = diamonds, mapping = aes(x = cut, y = carat)) +
  geom_boxplot()

### One problem with boxplots is that they were developed 
### in an era of much smaller datasets 
### and tend to display a prohibitively large number of “outlying values”. 
### One approach to remedy this problem is the letter value plot. 
### Install the lvplot package, and try using geom_lv() 
### to display the distribution of price vs cut. 
### What do you learn? How do you interpret the plots?

# The width of the "box" is dependent on number of values.
# The "box" gets thinner as carat gets bigger, meaning there are 
# many more diamonds with smaller carats (0 - 2), although
# carats spread all the way to 5

install.packages("lvplot")
library(lvplot)
ggplot(data = diamonds, mapping = aes(x = cut, y = carat)) +
  geom_lv()

?geom_lv()

### Compare and contrast geom_violin() with a facetted geom_histogram(), 
### or a coloured geom_freqpoly(). 
### What are the pros and cons of each method?

### STANDARDIZED = (u' - u) / SD + 1
install.packages("standardize")
library(standardize)

diamonds <- mutate(diamonds, ZCarat = scale(diamonds$carat))
?standardize()

plot1 <- ggplot(data = diamonds, mapping = aes(x = carat), binwidth = .1) +
  geom_histogram() +
  facet_grid(cut ~ .)

plot2 <- ggplot(data = diamonds, mapping = aes(x = cut, y = carat)) +
  geom_violin()

?facet_grid()

plot3 <- diamonds %>% 
  group_by(cut) %>%
  mutate(normcarat = carat/max(carat)) %>%
  ggplot(data = plot3, mapping = aes(x = normcarat, color = cut)) +
  geom_freqpoly()

install.packages("gridExtra")
library(gridExtra)
grid.arrange(plot1, plot2, plot3, ncol = 3)

?geom_violin()

### If you have a small dataset, it’s sometimes useful to use 
### geom_jitter() to see the relationship 
### between a continuous and categorical variable. 
### The ggbeeswarm package provides a number of methods 
### similar to geom_jitter(). 
### List them and briefly describe what each one does.

install.packages("ggbeeswarm")
library(ggbeeswarm)

?ggbeeswarm
ggplot(data = diamonds, mapping = aes(x = price, y = cut), alpha = 5) +
  geom_jitter()

ggplot(data = diamonds, mapping = aes(x = cut, y = carat)) +
  geom_quasirandom()

### How could you rescale the count dataset below 
### to more clearly show the distribution of cut within colour, 
### or colour within cut?

diamonds %>% 
  count(color, cut) %>% 
  group_by(color) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = prop)) +
  scale_fill_viridis(limits = c(0, 1))

install.packages("viridis")
library(viridis)

### Use geom_tile() together with dplyr to explore how 
### average flight delays vary by destination and month of year. 
### What makes the plot difficult to read? How could you improve it?

?reorder()
?factor()
?filter()

x <- c(1:5, 60:70)
factor(x)

nycflights %>%
  group_by(month, dest) %>%
  summarize(delays = mean(dep_delay, na.rm = TRUE)) %>%
  group_by(dest) %>%
  filter(n() == 12) %>%
  ungroup() %>%
  mutate(dest = reorder(dest, delays)) %>%
  ggplot(mapping = aes(x = factor(month), y = dest, fill = delays)) +
  geom_tile() +
  scale_fill_viridis()
  
### Why is it slightly better to use aes(x = color, y = cut) 
### rather than aes(x = cut, y = color) in the example above?

# Keep the longer words on the y-axis for easier readability

### Instead of summarising the conditional distribution with a boxplot, 
### you could use a frequency polygon. 
### What do you need to consider when using cut_width() vs cut_number()? 
### How does that impact a visualisation of the 2d distribution of carat and price?

ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_freqpoly(mapping = aes(group = cut_number(carat, 20)))

ggplot(data = diamonds, mapping = aes(color = cut_number(carat, 5), x = price)) +
  geom_freqpoly()

ggplot(data = diamonds, mapping = aes(group = cut_width(carat, .1), x = price)) +
  geom_freqpoly()

### Visualise the distribution of carat, partitioned by price.

diamonds %>%
  ggplot(data = diamonds, mapping = aes(color = cut_number(price, 10), x = carat)) +
  geom_boxplot()

### How does the price distribution of very large diamonds compare to small diamonds? 
### Is it as you expect, or does it surprise you?

x <- diamonds %>%
  group_by(carat) %>%
  filter(carat > 3 | carat < .3)

?facet_grid

ggplot(data = diamonds, mapping = aes(x = carat, y = price, color = cut_width(carat, .5))) +
  geom_violin()

### Combine two of the techniques you’ve learned to 
### visualize the combined distribution of cut, carat, and price.

ggplot(data = diamonds, mapping = aes(x = carat, y = price, color = cut)) +
  geom_violin() +
  facet_grid(cut ~ .)

ggplot(data = diamonds, mapping = aes(x = carat, y = price, color = cut, alpha = .1)) +
  geom_point() +
  facet_grid(cut ~ .)

install.packages("hexbin")
library(hexbin)

ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price)) +
  facet_grid(cut ~ .) +
  scale_fill_viridis()
