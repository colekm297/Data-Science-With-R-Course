library(tidyverse)

ggplot(diamonds, aes(carat, price)) + 
  geom_hex()
ggsave("IntroDataScience.pdf")

write_csv(IntroDataScience, "IntroDataScience.csv")

print(mtcars)
as_tibble(mtcars)
view(mtcars)

df <- data.frame(abc = 1, xyz = "a")
df$x
df[, "xyz"]
df[, c("abc", "xyz")]

tibble <- tibble(
  abc = 1,
  xyz = "a")

tibble$x
tibble[,"xyz"]
tibble[, c("abc", "xyz")]

annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)
annoying$'1'
annoying$'2'
?rnorm
?cor.test
cor.test(annoying$`1`, annoying$`2`)


annoying <- mutate(annoying, `3` = `2` / `1`)
print(annoying)

annoying <- rename(annoying, one = `1`, two = `2`, three = `3`)
?enframe()
?tibble()
deframe(enframe(1:3))

########### CHAPTER 11: DATA IMPORT ############

### Apart from file, skip, and comment, what other arguments do 
### read_csv() and read_tsv() have in common?

union(names(formals(read_csv)), names(formals(read_tsv)))

### What arguments do you need to specify to read the following text into a data frame?
### "x,y\n1,'a,b'"

x <- "x,y\n1,'a,b'"
read_csv(x, quote = "'")

### Identify what is wrong with each of the following inline CSV files. 
### What happens when you run the code?

# Unequal number of colnames
read_csv("a,b\n1,2,3\n4,5,6")

# Unequal number of values in columns, no treatment of NA
read_csv("a,b,c\n1,2\n1,2,3,4")

# Unequal number of values in column, no treatment of NA
read_csv("a,b\n\"1")

# No treatment of NA value
read_csv("a,b\n1,2\na,b")

# Needs to be read_csv2()
read_csv("a;b\n1;3")



union(names(formals(locale)), names(formals(locale)))

###What are the most important arguments to locale()?

# grouping_mark
# decimal_mark
# encoding
# date_format

### What happens if you try and set decimal_mark and grouping_mark to the same character? 
### What happens to the default value of grouping_mark when you set decimal_mark to “,”? 
### What happens to the default value of decimal_mark when you set the grouping_mark to “.”?

# Error
parse_number("1,000,000,000", locale = locale(grouping_mark = ","), locale = locale(decimal_mark = ","))

# Switches
parse_number("1.000,000", locale = locale(decimal_mark = ","))
parse_number("1.000,000", locale = locale(grouping_mark = "."))

### I didn’t discuss the date_format and time_format options to locale(). 
### What do they do? Construct an example that shows when they might be useful.

parse_time()
parse_datetime("")
?date_format()
?parse_datetime
?locale()
parse_date()

### If you live outside the US, create a new locale object 
### that encapsulates the settings for the types of file you read most commonly.

### What’s the difference between read_csv() and read_csv2()?

# read_csv2 is for the countries that use semi-colons to separate data rather than commas

### What are the most common encodings used in Europe? 
### What are the most common encodings used in Asia? Do some googling to find out.

# Traditional Chinese: Big5.
# Simplified Chinese: GB18030.
# Japanese: Shift-JIS, EUC-JP.
# Europe: ISO-8859 or ISO-8859-1

### Generate the correct format string to parse each of the following dates and times:
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14"
t1 <- "1705"
t2 <- "11:15:10.12 PM"

parse_date(d1, "%B %d, %Y")
parse_date(d2, "%Y-%b-%d")
parse_date(d3, "%d-%b-%Y")
parse_date(d4, "%B %d (%Y)")
parse_date(d5, "%m/%d/%y")
parse_time(t1, "%H%M")
parse_time(t2, "%H:%M:%OS %p")

### FORMATTING SHORTCUTS: 11.4 ###

############# CHAPTER 12 : WORKING WITH TIDY DATA ###############

### Compute the rate for table2, and table4a + table4b. 
### You will need to perform four operations:
### Extract the number of TB cases per country per year.
### Extract the matching population per country per year.
### Divide cases by population, and multiply by 10000.
### Store back in the appropriate place.
### Which representation is easiest to work with? Which is hardest? Why?

table2
cases <- table2 %>%
  filter(type == "cases") %>%
  rename(cases = count)

population <- table2 %>%
  filter(type == "population") %>%
  rename(population = count)

table2.1 <- tibble(
  year = cases$year,
  country = cases$country,
  cases = cases$cases,
  population = population$population,
  rate = cases / population
)

table4c <- tibble(
  country = table4a$country,
  '1999' = table4a$`1999` / table4b$`1999`,
  '2000' = table4a$`2000` / table4b$`2000`
)

stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)

stocks %>% 
  pivot_wider(names_from = year, values_from = return)

stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return", names_ptype = list(year = double()))

table4a

table4a %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")

### What would happen if you widen this table? 
### Why? How could you add a new column to uniquely identify each value?

people <- tribble(
  ~name,             ~names,  ~values,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)

people2 <- people %>% 
  group_by(name, names) %>%
  mutate(obs = row_number(names))

pivot_wider(people2, names_from = names, values_from = values)

### Tidy the simple tibble below. 
### Do you need to make it wider or longer? 
### What are the variables?

preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)

preg %>% 
  pivot_longer(c("male", "female"), names_to = "gender", values_to = "scores", values_drop_na = TRUE)


### What do the extra and fill arguments do in separate()? 
### Experiment with the various options for the following two toy datasets.

?separate()

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"), extra = "drop")

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
  separate(x, c("one", "two", "three"), fill = "warn")

### Both unite() and separate() have a remove argument. 
### What does it do? Why would you set it to FALSE?

# You can double-check that separate worked correctly.
# It gives you the values of the original tibble in a column

?extract()

who2 <- who %>%
  pivot_longer(
    cols = new_sp_m014:newrel_f65, 
    names_to = "key", 
    values_to = "cases", 
    values_drop_na = TRUE
  ) %>% 
  mutate(
    key = stringr::str_replace(key, "newrel", "new_rel")
  ) %>%
  separate(key, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)

view(who)

select(who, country, iso2, iso3) %>%
  distinct() %>%
  group_by(country) %>%
  filter(n() > 1)

who1 <- who %>%
  count(country)

### For each country, year, and sex compute the total number of cases of TB. 
### Make an informative visualisation of the data.

who2 %>%
  group_by(country, year, sex) %>%
  filter(year > 1995) %>%
  summarise(cases = sum(cases)) %>%
  unite(country_sex, country, sex, remove = FALSE) %>%
  ggplot(aes(x = year, y = cases, group = country_sex, color = sex)) +
  geom_line()

### We know that some days of the year are “special”, and fewer people than usual fly on them. 
### How might you represent that data as a data frame? 
### What would be the primary keys of that table? 
### How would it connect to the existing tables?
# COME BACK TO THIS ^^^^

### Add a surrogate key to flights.

flights <- flights %>%
  mutate(key = row_number())

### Identify the keys in the following datasets
install.packages("babynames")
install.packages("nasaweather")
install.packages("fueleconomy")

view(Lahman::Batting)

Lahman::Batting %>%
  count(playerID, yearID, stint) %>%
  filter(n > 1)


view(babynames::babynames)
babynames::babynames %>%
  count(name, sex, year) %>%
  filter(n > 1)


view(nasaweather::atmos)
nasaweather::atmos %>%
  count(lat, long, year, month) %>%
  filter(n > 1)

view(fueleconomy::vehicles)
fueleconomy::vehicles %>%
  count(id) %>%
  filter(n > 1)

view(ggplot2::diamonds)
ggplot2::diamonds %>%
  count(x, y, z, depth, price, clarity, cut, carat, color, table) %>%
  filter(n > 1)

AwardsManagers <- view(Lahman::AwardsManagers)
Master <- view(Lahman::Master) 
Salaries <- view(Lahman::Salaries)

### Compute the average delay by destination, 
### then join on the airports data frame so you can show the spatial distribution of delays.

airports <- nycflights13::airports

flights %>%
  group_by(dest) %>%
  filter(arr_delay > 0) %>%
  mutate(avg_delay = mean(arr_delay)) %>%
  left_join(airports, c("dest" = "faa")) %>%
  ggplot(aes(lon, lat, color = avg_delay)) +
  borders("state") +
  geom_point(na.rm = TRUE) +
  coord_quickmap() + 
  scale_color_viridis()

install.packages("maps")
library(maps)
library("viridis") 
airports <- nycflights13::airports

airports2 <- airports %>%
  semi_join(flights2, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat, color = avg_delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap()
  

### Add the location of the origin and destination (i.e. the lat and lon) to flights.

flights %>%
  left_join(airports, c("dest" = "faa")) %>%
  left_join(airports, c("origin" = "faa")) %>%
  glimpse()

view(nycflights13::planes)

### Is there a relationship between the age of a plane and its delays?

# Doesn't seem so: 

flightdelays <- flights %>%
  group_by(tailnum) %>%
  inner_join(planes, by = "tailnum") %>%
  mutate(age = 2013 - year.y) %>%
  group_by(age) %>%
  summarise(delays = mean(arr_delay, na.rm = TRUE))
  
ggplot(flightdelays, aes(age, delays)) +
  geom_point(na.rm = TRUE) +
  geom_smooth()

### What weather conditions make it more likely to see a delay?

# Too lazy, come back

view(nycflights13::weather)

flights2 <- flights %>%
  left_join(weather) %>%
  mutate(delays = mean(arr_delay))
  
### What happened on June 13 2013? 
### Display the spatial pattern of delays, 
### and then use Google to cross-reference with the weather.


flights %>%
  group_by(dest) %>%
  filter(month == 6, day == 13) %>%
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE)) %>%
  inner_join(airports, c("dest" = "faa")) %>%
  ggplot(aes(lon, lat, color = avg_delay, size = avg_delay)) +
  borders("state") +
  geom_point(na.rm = TRUE) +
  coord_quickmap() + 
  scale_color_viridis()

### What does it mean for a flight to have a missing tailnum? 
### What do the tail numbers that don’t have a matching record 
### in planes have in common? (Hint: one variable explains ~90% of the problems.)

missing <- flights %>%
  anti_join(planes, by = "tailnum")

### Filter flights to only show flights with planes that have flown at least 100 flights

HundoFlights <- flights %>%
  filter(!is.na(tailnum))
  group_by(tailnum) %>%
  count() %>%
  filter(n >= 100)

flights %>%
  semi_join(HundoFlights, by = "tailnum")

### Combine fueleconomy::vehicles and fueleconomy::common to find 
### only the records for the most common models.

vehicles <- view(fueleconomy::vehicles)
common <- view(fueleconomy::common)

vehicles %>%
  semi_join(common, by = c("make", "model"))

### Find the 48 hours (over the course of the whole year) 
### that have the worst delays. 
### Cross-reference it with the weather data. Can you see any patterns?

worstdelays <- flights %>%
  mutate(hour = sched_dep_time %/% 100) %>%
  group_by(origin, year, month, day, hour) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(delay)) %>%
  slice(1:48)

weather_most_delayed <- semi_join(weather, worstdelays, by = c("origin", "year", "month", "day", "hour"))

select(weather_most_delayed, temp, wind_speed, precip)

### What does anti_join(flights, airports, by = c("dest" = "faa")) tell you? 
### What does anti_join(airports, flights, by = c("faa" = "dest")) tell you?

# Foreign flights
# Airports that weren't flown to from NY

### You might expect that there’s an implicit relationship between plane and airline, 
### because each plane is flown by a single airline. 
### Confirm or reject this hypothesis using the tools you’ve learned above.

view(nycflights13::airlines)
?distinct()

flights %>%
  anti_join(airlines, by = "carrier") %>%
  distinct()

distinct <- flights %>%
  filter(!is.na(tailnum)) %>%
  group_by(tailnum) %>%
  distinct(carrier)

distinct %>%
  count(tailnum) %>%
  filter(n > 1)
