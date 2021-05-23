library(tidyverse)
library(caret)

set.seed(1, sample.kind="Rounding")
n <- c(100, 500, 1000, 5000, 10000)
f <- function(n) {
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1), 2, 2)
  dat <- MASS::mvrnorm(n=n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
    test <- dat %>% slice(test_index)
    train <- dat %>% slice(-test_index)
    fit <- lm(y~x, data=train)
    y_hat <- predict(fit, test)
    sqrt(mean((y_hat-test$y)^2))
  })
  print(c(n = as.integer(n), avg = mean(rmse), se =sd(rmse)))
}

sapply(n, f)

