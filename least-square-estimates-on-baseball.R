library(Lahman)
library(tidyverse)
bat_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>% summarize(mean_singles=mean(singles),
                                   mean_bb=mean(bb))
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

#number of players with single rate and bb rate average greater than 0.2 over the period 1999 - 2001 
sum(bat_01$mean_singles>0.2)
sum(bat_01$mean_bb>0.2)

bat <- inner_join(bat_01, bat_02, by="playerID")

cor(bat$mean_singles, bat$singles)
cor(bat$mean_bb, bat$bb)

#scatterplots to see it the distributions are bivariate normal
p_01 <- bat %>% ggplot(aes(mean_singles, singles)) + geom_point(alpha=0.5)
p_01
p_02 <- bat %>% ggplot(aes(mean_bb, bb)) +geom_point(alpha=0.5)
p_02

#prediction of 2002 singles and bb using 1999-2001 data
lm(singles~mean_singles, data=bat)
lm(bb~mean_bb, data=bat)
