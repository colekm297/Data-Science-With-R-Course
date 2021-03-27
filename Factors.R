library(tidyverse)

ggplot(gss_cat, aes(rincome)) +
  geom_bar() +
  coord_flip()

gss_cat %>%
  count(relig)

gss_cat %>%
  count(partyid)

x3 <- semi_join(x = gss_cat, y = x2)

x2 <- gss_cat %>%
  count(relig, n() > 10)

gss_cat %>%
  group_by(relig) %>%
  summarise(
    count = n()
  ) %>%
  filter(count > 100) %>%
  ungroup() 

popular_relig <- gss_cat %>%
  group_by(relig) %>%
  filter(n() > 100)

ggplot(popular_relig, aes(relig, fill = denom)) +
  geom_bar(show.legend = FALSE) +
  coord_flip()

### 15.4.1 Exercises ###

outliersremoved <- gss_cat %>%
  group_by(tvhours) %>%
  filter(tvhours < 20) %>%
  ungroup()

mean(outliersremoved$tvhours)
median(gss_cat$tvhours, na.rm = TRUE)

levels(gss_cat$tvhours)

mean(gss_cat$tvhours, na.rm = TRUE)

### 15.5.1 Exercises ###

gss_cat %>%
  group_by(partyid) %>%
  count(n())

newpartyid <- gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
    other = c("No answer", "Don't know", "Other party"),
    dem = c("Strong democrat", "Not str democrat"),
    independent = c("Ind,near dem", "Independent", "Ind,near rep"),
    repub = c("Not str republican", "Strong republican")
  )) %>%
  group_by(partyid, year) %>%
  mutate(count = n()) %>%
  mutate(prop = count/sum(count))

  ggplot(newpartyid, aes(year, prop, color = partyid)) +
  geom_line(na.rm = TRUE) +
  ylim(0, .0025)

gss_cat %>%
  mutate(rincome = fct_lump(rincome, n = 5)) %>%
  count(rincome)
