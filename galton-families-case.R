library(tidyverse)
library(HistData)
data("GaltonFamilies")
galton_heights <- GaltonFamilies %>%
  filter(childNum==1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))
galton_heights %>% ggplot(aes(father, son))+ geom_point(alpha=0.5)
galton_heights %>% summarise(r=cor(father, son))

lm(son~father, data=galton_heights)

fit <- galton_heights %>% lm(son ~ father, data = .) 
Y_hat <- predict(fit, se.fit = TRUE)
Y_hat
