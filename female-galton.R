set.seed(1989)
library(HistData)
library(tidyverse)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)
mu_x <- mean(female_heights$mother)
mu_x
sd_x <- sd(female_heights$mother)
sd_x
mu_y <- mean(female_heights$daughter)
mu_y
sd_y <- sd(female_heights$daughter)
sd_y
r <- cor(female_heights$mother, female_heights$daughter)
r
slope_y <- r*sd_y/sd_x
slope_y
interc_y <- mu_y - mu_x*slope_y
interc_y
pvarex_y <- r^2*100
pvarex_y
#Case : 60 inches mother prediction
interc_y + slope_y*60
