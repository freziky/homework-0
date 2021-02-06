set.seed(1989)
library(HistData)
data("GaltonFamilies")
options(digits = 3)

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)
linear <- lm(mother~daughter, data=female_heights)

femalepred <- female_heights %>% mutate(moth_hat=linear$coefficients[1]+daughter*linear$coefficients[2])
